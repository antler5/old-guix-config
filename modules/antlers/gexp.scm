(define-module (antlers gexp)
  #:use-module (guix gexp)
  #:export (append-to-plain-file))

(define (append-to-plain-file file . lines)
  (plain-file (plain-file-name file)
    (string-append
      (plain-file-content file)
      (string-join lines "\n" 'suffix))))
