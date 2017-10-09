(load "let.scm")


(define (test)
  (run "letrec break (x) = (break x) in let f = proc (y) 11 in (f (break 1))"))

