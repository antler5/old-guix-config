#!/usr/bin/env -S guix shell guile-json openssl -- guile -L ./modules
!#

(add-to-load-path (string-append (getenv "GUIX_ENVIRONMENT")
                                 "/share/guile/site/3.0"))

(define-module (sops)
  #:use-module (antlers sops)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1))

(define guix-sops-version "0.0.1")

(define config
  (with-input-from-file "./.sops.json"
    (lambda _ (json->scm))))

(define (sops-user-data dict)
  (filter (lambda (pair)
            (not (or (equal? "sops" (car pair))
                     (string-suffix? "_unencrypted" (car pair)))))
          dict))

(define (ssh->pem pubkey)
  (invoke-pipeline `(("ssh-keygen" "-f" "/dev/stdin" "-e" "-m" "pem"))
                   #:input-strings `(,pubkey)))

(define (encrypt type key data)
  (cond ((equal? type "ssh_rsa")
         (let ((pubkey (ssh->pem key)))
           (invoke-pipeline
             `(,(read-into "pubkey" pubkey
                  `("openssl" "pkeyutl" "-encrypt" "-inkey" "<(cat <<< \"$pubkey\")" "-pubin" "-in" "/dev/stdin"))
               ("base64")
               ("xargs" "-0" "printf" "-----BEGIN RSA MESSAGE-----\n%s-----END RSA MESSAGE-----"))
             #:input-strings `(,pubkey ,data))))
        ((equal? type "passphrase")
         (invoke-pipeline `(,(read-into "data_file" data
                               `(,@gpg-cmd "--passphrase-fd" "0"
                                          "--symmetric" "--output" "-" ; stdout
                                          "<(cat <<< \"$data_file\")")))
                          #:input-strings `(,data ,key)))
        ((equal? type "pgp")
         (invoke-pipeline `((,@gpg-cmd "--encrypt" "--recipient" ,key))
                          #:input-strings `(,data)))))

(define (smudge-file in-file out-file)
  (let* ((dict (with-input-from-file in-file (lambda _ (json->scm))))
         (data-key #f))
    (unless (assoc-ref (assoc-ref dict "sops") "pgp")
      (error "No PGP recipient found."))
    (let loop ((tail (vector->list (assoc-ref (assoc-ref dict "sops") "pgp"))))
      (unless (null? tail)
        (let ((try-decrypt (decrypt (assoc-ref (car tail) "enc"))))
          (if (string? try-decrypt)
              (set! data-key try-decrypt)
              (loop (cdr tail))))))

    (when (null? data-key)
      (error "Unable to decrypt."))

    (with-output-to-file out-file
      (lambda _
        (scm->json
          (walk-json-dict dict
            (lambda (obj)
              (decrypt obj data-key)))
          #:pretty #t)))))

(define (create-data-key)
  (invoke-pipeline '(("openssl" "rand" "-base64" "256"))))

(define (clean-file in-file out-file)
  (let* ((dict (with-input-from-file in-file (lambda _ (json->scm))))
         (data-key (create-data-key))
         (creation-rules
           (find (lambda (r)
                   (string-match (assoc-ref r "path_regex")
                                 (string-drop out-file (string-prefix-length out-file "./"))))
                 (vector->list (assoc-ref config "creation_rules"))))
         (recipients (car (vector->list (assoc-ref creation-rules "key_groups"))))
         (sops-data '()))

    (for-each (match-lambda
                ((enc-type . pubkeys)
                 (for-each (lambda (pubkey)
                             (let ((enc-data (encrypt enc-type pubkey data-key)))
                               (when enc-data
                                 (set! sops-data
                                       (assoc-set! sops-data enc-type
                                                   (cons `(("enc" . ,(encrypt enc-type pubkey data-key))
                                                           ("recipient" . ,pubkey))
                                                         (or (assoc-ref sops-data enc-type) '())))))))
                           (vector->list pubkeys))
                 (when (assoc-ref sops-data enc-type)
                   (set! sops-data
                         (assoc-set! sops-data enc-type
                                     (list->vector (assoc-ref sops-data enc-type)))))))
              recipients)
    (set! sops-data (assoc-set! sops-data "version" guix-sops-version))
    (set! sops-data (assoc-set! sops-data "lastmodified" (strftime "%Y-%m-%dT%H:%M:%SZ" (localtime (current-time)))))
    (set! sops-data (acons "sops" sops-data '()))

    (set! sops-data
      (append sops-data
              (walk-json-dict dict
                (lambda (obj)
                  (encrypt "passphrase" data-key obj)))))

    (with-output-to-file out-file
      (lambda _ (scm->json sops-data) #:pretty #t))))

(define* (edit-file filename)
  (let ((tmpfile (mkstemp (string-append filename ".XXXXXX"))))
    (when (file-exists? filename)
      (smudge-file filename (port-filename tmpfile)))

    (dynamic-wind
      (const #t)
      (lambda _
        (with-input-from-file "/dev/tty"
          (lambda _ (system* (or (getenv "EDITOR") "vi") (port-filename tmpfile))))
        (clean-file (port-filename tmpfile) filename))
      (lambda _
        (delete-file (port-filename tmpfile))))))

(edit-file (second (command-line)))
