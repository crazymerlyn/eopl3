(load "let.scm")


(define (test)
  (run "let x = 100 in let y = proc (z) set x = -(x,z)
        in let d = spawn (proc () (y 1)) in x"))

