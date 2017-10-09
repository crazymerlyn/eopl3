(load "let.scm")


(define (test)
  (run "let x = 1 in begin {set x = -(x,1); x}"))

