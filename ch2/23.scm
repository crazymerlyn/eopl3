(define-datatype
  lc-exp lc-exp?
  (var-exp
    (var lc-exp-identifier?))
  (lambda-exp
    (bound-var lc-exp-identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(define (lc-exp-identifier? sym)
  (and (identifier? sym)
       (not (eq? sym 'lambda))))
