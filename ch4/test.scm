(load "let.scm")


(define (test)
  (run "let swap = proc (x, y) let temp = x in begin {set x = y; set y = temp}
        in let a = 1 b = 2 in begin {(swap a b); -(a,b)}"))

