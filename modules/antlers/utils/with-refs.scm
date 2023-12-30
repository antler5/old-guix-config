(define-module (antlers utils with-refs)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (with-refs))

(define *unset* (symbol-append (gensym) '-unset))

;; For use in macro expansions
(define (module-publicly-bound?-exdr m v)
  '(module-bound? (module-public-interface ,m) ,v))

;; For use at compile-time
(define (module-publicly-bound? m v)
  (module-bound? (module-public-interface m) v))

;;; `clause` record-type
(define-record-type <clause>
  (make-clause* export? module-name variable-name value
                module-value internal-value external-value)
  clause?
  (export?        clause-export?)
  (module-name    clause-module-name)
  (variable-name  clause-variable-name)
  (value          clause-value)
  ;; These 3 are gensyms, which become bindings in the macro expansion.
  (module-value   clause-module-value)
  (internal-value clause-internal-value)
  (external-value clause-external-value))

;; Defines gensym fields automatically.
(define (make-clause . args)
  (apply make-clause*
    (append args (map (cute symbol-append (gensym) <>)
                      '(-module-value -internal-value -external-value)))))

(define sexp->clause
  (match-lambda
    (((export? module-name variable-name) value)
     (set! export? (match export? ('@@ #t) ('@ #f)))
     (make-clause export? module-name variable-name value))
    ((variable-name value)
     (make-clause (module-publicly-bound? (module-name (current-module)) variable-name)
                  (module-name (current-module)) variable-name value))))

;;; Expansion Bindings
;; This un-hygenic macro can be used as an accessor when `clause` is
;; bound in it's expansion environment.
(defmacro clause-prop (prop)
  `(,(symbol-append 'clause '- prop) clause))

(define (clause->bindings clause)
  `((,(clause-prop module-value)   (resolve-module ',(clause-prop module-name)))
    (,(clause-prop internal-value) ,(clause-prop value))
    (,(clause-prop external-value) #f)))

(define (store-value clause external?)
  (set! external? (match external? (#:external #t) (#:internal #f)))
  `(set! ,(if external? (clause-prop external-value)
                        (clause-prop internal-value))
          (if (module-bound? ,(clause-prop module-value) ',(clause-prop variable-name))
              (module-ref ,(clause-prop module-value) ',(clause-prop variable-name))
              ',*unset*)))

(define* (set-value clause external?)
  (set! external? (match external? (#:external #t) (#:internal #f)))
  `(if (and (equal? ',*unset* ,(if external? (clause-prop external-value)
                                             (clause-prop internal-value)))
            (module-variable ,(clause-prop module-value)
                             ',(clause-prop variable-name)))
       (variable-unset! (module-variable ,(clause-prop module-value)
                                         ',(clause-prop variable-name)))
       (module-define! ,(clause-prop module-value)
                       ',(clause-prop variable-name)
                       ,(if external? (clause-prop external-value)
                                      (clause-prop internal-value)))))

;; Dynamic Wind
(define (with-refs-expander clauses* body)
  (define clauses '())
  (map (lambda (clause) (set! clauses (cons clause clauses)))
       (map sexp->clause clauses*))
  `(let ,(append-map clause->bindings clauses)
     (dynamic-wind
       (lambda () ,@(append (map (cut store-value <> #:external) clauses)
                            (map (cut set-value <> #:internal) clauses)))
       (lambda () ,body)
       (lambda () ,@(append (map (cut store-value <> #:internal) clauses)
                            (map (cut set-value <> #:external) clauses))))))

;; Macro Chrome
(define-macro (with-refs clauses expr . rest)
  (apply with-refs-expander `(,clauses ,expr . ,rest)))

;; TODO: Real tests
;; For testing:
;; (use-modules (ice-9 pretty-print))
;; (pretty-print (with-refs-expander '(((@@ (test test-1) test-var) 'test-val)) '((@@ (test test-1) test-var))))
;; (with-refs (((@@ (test test-1) test-var) 'test-val)) (@@ (test test-1) test-var))
