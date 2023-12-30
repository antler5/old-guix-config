(define-module (antlers sops)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (gpg-cmd
            invoke-pipeline
            read-into
            decrypt
            walk-json-dict))

(define gpg-cmd
  `("gpg" "--quiet" "--batch" "--status-fd" "--with-colons" "--armor"))

(define* (invoke-pipeline argvs #:optional #:key input-strings)
  (with-output-to-file "/persist/LOG3" (lambda _ (pretty-print (list argvs input-strings))))
  (receive (from to pids) (pipeline argvs)
    (when input-strings
      (put-string to (apply string-append input-strings)))
    (close to)
    (let* ((output (get-string-all from))
           (_      (close from))
           (success? (lambda (pid)
                       (zero? (status:exit-val (cdr (waitpid pid))))))
           (index (list-index (negate success?) (reverse pids))))
      (when index
        (error "command failed: "
               (string-join (list-ref argvs index))))
      (string-drop-right output (string-suffix-length output "\n")))))

(define (read-into name var argv)
  `("bash" "-c"
    ,(string-join `("read" "-N" ,(number->string (string-length var)) ,name ";"
                    ,@argv))))

(define* (decrypt secret #:optional passphrase)
  (let ((maybe-passphrase
          (lambda (argv)
            (if passphrase (read-into "secret" secret argv) argv))))
      (invoke-pipeline
        `(,(maybe-passphrase
          `(,@gpg-cmd "--decrypt"
                     ,@(if passphrase
                           `("--pinentry-mode" "loopback"
                             "--passphrase-fd" "0"
                             ,(string-append "<(cat <<< \"$secret\")"))
                           '()))))
        #:input-strings `(,secret ,@(if passphrase `(,passphrase) '())))))

(define (walk-json-dict dict func)
  (let ((sops-data? (lambda (x) (not (and (pair? x) (equal? (car x) "sops"))))))
    (let loop ((obj dict))
      (cond ((string? obj)
             (func obj))
            ((pair? (car obj))
             (map loop (filter sops-data? obj)))
            ((pair? obj)
             (let ((key (car obj))
                   (val (cdr obj)))
               (cons key
                     (cond ((string? val) (func val))
                           ((number? val) (func (number->string val)))
                           ((vector? val) (apply vector (map loop (vector->list val))))
                           ((list?   val) (map loop val))))))))))

