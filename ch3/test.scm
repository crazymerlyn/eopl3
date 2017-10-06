(load "let.scm")


(define (test)
  (run "let x = 1 in let y = proc (z, y) -(z, y) in (y x 4)"))

