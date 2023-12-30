(define-module (antlers partition partition-table)
  #:use-module (antlers partition tree)
  #:use-module (antlers records)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system uuid)
  #:use-module (rnrs bytevectors)
  #:export (mount-point
            offset
            partition-table
            partition))

;; STATUS: Out of date.
(define-record-type* <mount-point> mount-point*
  ;; The location of a mount-point must be specified as a path
  ;; and, except when describing the location of a block-device under
  ;; `/dev/…', the path or label of the underlying block-device.
  mount-point**
  mount-point?
  this-mount-point

  (path mount-point-path)    ; <file-system-label> or path as <string>
  (parent mount-point-parent ; <string> | #f
          (default #f)))

(define (mount-point . args)
  (apply mount-point**
    (append args (map (const #f)
                      (iota (- (length (record-type-fields <mount-point>))
                               (length args)))))))

(define-record-type* <partition-table> partition-table
  make-partition-table
  partition-table?
  this-partition-table

  (id           partition-table-id            ; <mount-point>
                (thunked)
                (default (partition-table-block-device
                           this-partition-table)))
  (block-device partition-table-block-device) ; path as <mount-point>
  (type         partition-table-type)         ; 'gpt
  (partitions   partition-table-partitions)   ; <list-of-partitions>
  (->nodes      partition-table-transformer
                (default partition-table->nodes)))

(define-record-type* <partition> partition
  make-partition
  partition?
  this-partition

  (name partition-name)   ; <string>
  (flags partition-flags  ; <list of symbols>
         (default '()))
  (start partition-start) ; <offset>
  (end partition-end))    ; <offset>

(define-record-type* <offset> offset*
  offset
  offset?
  this-offset

  (value offset-value) ; <int>
  (unit  offset-unit)) ; <symbol>

(define (offset->string offset)
  (string-append (number->string (offset-value offset))
                 (symbol->string (offset-unit offset))))

(define (partition-table->gexp partition-table)
  #~(system*
      #$@(cons* (file-append (specification->package "parted") "/sbin/parted")
                "--script" "--align=optimal" "--"
                (mount-point-path (partition-table-block-device partition-table))
                "mklabel " (symbol->string (partition-table-type partition-table))
                (let ((i 0))
                  (append-map
                    (lambda (partition)
                      (let ((i     (number->string (begin (set! i (1+ i)) i)))
                            (start (offset->string (partition-start partition)))
                            (end   (offset->string (partition-end partition))))
                        `("mkpart" "primary" ,start ,end
                          "name" ,i ,(partition-name partition)
                          ,@(append-map (lambda (flag)
                                          `("set" ,i ,(symbol->string flag) "on"))
                                        (partition-flags partition)))))
                    (partition-table-partitions partition-table))))))

(define (object->uuid object)
  (let ((state (seed->random-state (object->string object))))
    ((compose uuid->string bytevector->uuid u8-list->bytevector)
     (map (lambda _ (random 256 state)) (iota 32)))))

(define (partition-table->node partition-table)
  (node (provides (mount-point (object->uuid (partition-table-id partition-table))
                               (mount-point-parent
                                 (partition-table-block-device partition-table))))
        (requires (apply list (or (mount-point-parent
                                    (partition-table-block-device
                                      partition-table))
                                  '())))
        (snippet (partition-table->gexp partition-table))))

(define (partition-table->nodes partition-table)
  (let ((partition-table-node (partition-table->node partition-table)))
    (cons partition-table-node
          (map (lambda (partition)
                 (node (provides (mount-point (partition-name partition)))
                       (requires (list (node-provides partition-table-node)))))
               (partition-table-partitions partition-table)))))
