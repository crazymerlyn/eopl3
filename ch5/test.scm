(load "let.scm")


(define (test)
  (run "letrec times4 (n) = if zero?(n) then 0 else -((times4 -(n,1)),-4) in (times4 10)"))

