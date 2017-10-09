(load "let.scm")


(define (test)
  (run "let x = cons(1,2) in begin {set-car! (x, 3); cdr(x)}"))

