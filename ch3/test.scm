(load "let.scm")


(define (test)
  (run "let x=proc (y, z) -(y, z) in (x 12 1)"))

