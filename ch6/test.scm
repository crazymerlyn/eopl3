(load "let.scm")


(define (test)
  (run "letrec even (x) = if zero? (x) then 1 else (odd -(x,1))
               odd (x)  = if zero? (x) then 0 else (even -(x,1))
        in (even 19)"))

