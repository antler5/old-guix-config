(define-module (antlers partition file-systems)
  #:use-module (antlers partition tree)
  #:use-module (antlers records)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:export (file-system*))

(define-record-type* <file-system*> file-system*
  make-file-system*
  file-system*?
  this-file-system*

  (type file-system*-type) ; string
  (->nodes file-system*-transformer
           (default file-system*->nodes))
  )

(define (file-system->gexp file-system)
  #~(system* #$(file-append (specification->package "e2fsprogs") "/sbin/mke2fs"))
  )

(define (file-system*->nodes file-system*)
  (list (node (provides (gensym))
              (requires '())
              (snippet (file-system->gexp file-system)))))

