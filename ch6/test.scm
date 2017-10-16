(load "type.scm")


(define (test)
  (type-of-program (scan&parse "let a = 5 in zero?(a)")))

