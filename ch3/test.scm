(load "let.scm")


(define (test)
  (run "letrec times (x, y) = if zero?(x) then 0 else -((times -(x,1) y), -(0,y))
        in (times 10 10)"))

