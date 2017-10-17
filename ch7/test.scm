(load "type.scm")


(define (test)
  (types-to-external-form (type-of-program (scan&parse "module m interface [x : int] body [x = 4] from m take x"))))

