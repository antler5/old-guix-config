(define-module (antlers partition tree)
  #:use-module (antlers records)
  #:use-module (antlers records)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (node
            node-provides))

(define-record-type* <node> node
  make-node
  node?
  this-node

  (provides node-provides       ; <mount-point> | <file-system-label> | <symbol>
            (default '()))
  (requires node-requirements   ; list of the above
            (default '()))
  (required-by node-required-by ; ditto
               (default '()))

  (snippet node-snippet         ; <gexp>
           (default #t))
  (idempotent? node-idempotent? ; bool
               (default #f))
  )

;; STATUS: Out of date.
(define (nodes->tree nodes)
  (map (lambda (node)
         (modify-record node
           (requires -> (map (lambda (node-id) (find (lambda (n) (equal? (node-provides n) node-id)) nodes)) <>))))
    nodes))

;; STATUS: Placeholder.
(define (tree->gexps tree)
  #f)

;; STATUS: Roadmap.
;; (define (run-nodes nodes)
;;   #~(begin #$@(map node-)))

