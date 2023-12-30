(define-module (antlers records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-64)
  #:use-module (guix records)
  #:use-module (ice-9 common-list)
  #:export (modify-record))

;;
;; - No compile-time checks because `(guix records)' scares me.
;;
;; - Composes chains of functions that map over record fields;
;;   Theoretically it'd be better to bucket them by field, `compose'
;;   each bucket, then map just once. Consider execution order undefined.
;;

;; XXX: Should we make a special-case to avoid recurring into cut(e) forms, or
;; can anything clever be done to support nesting? (Custom delimiter, maybe in
;; the obarray to support renaming on import?)
(define-syntax deep-cut
  (lambda (stx)
    (syntax-case stx ()
      ((_ expr ...)
       (with-syntax (((value) (generate-temporaries '(value))))
         #`(lambda (value)
             #,@(let e-loop ((exprs #'(expr ...))
                             (e-acc '()))
                  (if (null? exprs)
                      (reverse e-acc)
                      (let ((e-head (car exprs))
                            (e-tail (cdr exprs)))
                        (syntax-case e-head (<> <...> modify-record)
                          ;; We're just gonna special-case recursion.
                          ((modify-record record body ...)
                           (e-loop e-tail (cons #`(modify-record #,@(e-loop #'(record) '()) body ...) e-acc)))
                          (<>
                           (e-loop e-tail (cons #'value e-acc)))
                          ((t1 t2 ... <...>)
                           (e-loop e-tail (cons #`(apply t1 #,@#'(t2 ...) value) e-acc)))
                          ;; XXX: Can't do unbounded tails.
                          ;; ((t1 t2 ... <...> . (rest))
                          ;;  (e-loop e-tail (cons #`(apply t1 #,@#'(t2 ...) (reverse (cons rest (reverse value)))) e-acc)))
                          ((t1 ...) (e-loop e-tail (cons (e-loop #'(t1 ...) '()) e-acc)))
                          (else     (e-loop e-tail (cons e-head e-acc)))))))))))))

(define (%modify-record record type field exprs wrapped?)
  #`(lambda (r)
      (apply (record-constructor #,type)
             (map (lambda (f)
                    (let ((value ((record-accessor #,type f) r)))
                      (if (eq? f '#,field)
                          #,(case wrapped?
                              ((thunked) #`(lambda args ((deep-cut #,@exprs) (apply ((record-accessor #,type f) r) args))))
                              ((delayed) #`(delay ((deep-cut #,@exprs) (force ((record-accessor #,type f) r)))))
                              (else      #`((deep-cut #,@exprs) value)))
                          value)))
                  (record-type-fields #,type)))))

(define-syntax modify-record
  (lambda (stx)
    (syntax-case stx ()
      ((_ record clause ...)
       (let c-loop ((clauses #'(clause ...))
                    (modifiers '()))
         (if (null? clauses)
             (with-syntax (((r) (generate-temporaries '(r))))
               #`(let* #,(cons #'(r record)
                               (map (lambda (m) #`(r (#,m r)))
                                    (reverse modifiers)))
                    r))
             (let ((c-head (car clauses))
                   (c-tail (cdr clauses)))
               (c-loop c-tail
                 (cons (apply %modify-record
                              (cons* #'record
                                     #'(record-type-descriptor record)
                                     (syntax-case c-head (-> => -->)
                                       ((field ->  rest ...) (list #'field #'(rest ...) #f))
                                       ((field =>  rest ...) (list #'field #'(rest ...) 'thunked))
                                       ((field --> rest ...) (list #'field #'(rest ...) 'delayed)))))
                       modifiers)))))))))

(define (test)
  (set! test-log-to-file #f)
  (test-begin "deep-cut")
  (test-eqv ((deep-cut <>) 'a) 'a)
  (test-equal ((deep-cut (list 'a <>))              'b)     '(a b))
  (test-equal ((deep-cut (list 'a 'b (list <> 'd))) 'c)     '(a b (c d)))
  (test-equal ((deep-cut (list 'a 'b <>))           '(c d)) '(a b (c d)))
  (test-equal ((deep-cut (list 'a 'b <...>))        '(c d)) '(a b c d))
  (test-equal ((deep-cut (list 'a 'b (list <...>))) '(c d)) '(a b (c d)))
  (test-equal ((deep-cut (list 'a 'b (cons <...>))) '(c d)) '(a b (c . d)))
  ; XXX: I could probably do this with two passes.
  ; (test-equal ((deep-cut (list 'a 'b (list 1 2 <...> 'e) 'f)) (list '(c) 'd)) '(a b (1 2 (c) d e) f))
  ; (test-equal ((deep-cut (list 'a 'b (list 'c 'd <...> 'g 'i) 'j 'k)) (list '(e) 'f)) '(a b (c d (e) f g h) i j))
  (test-end "deep-cut"))

