(define-syntax define-via-indirection
  (lambda (x)
    (syntax-case x ()
      ((_)
       (with-syntax ((test (datum->syntax x 'test)))
         #'(define test 'needle))))))
  (define-via-indirection)

  (module-map (lambda (sym val)
                (when (eq? (variable-ref val) 'needle)
                  (display sym)
                  (newline)))
              (resolve-module '(guile-user))))
;; => test-b5eb162ed7629fe
