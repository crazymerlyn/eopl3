(load "let.scm")


(define (test)
  (run "letrec times4 (x) = if zero?(x) then 0 else -((times4 -(x,1)), -4)
        in (times4 100)"))

