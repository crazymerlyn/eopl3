(load "let.scm")


(define (test)
  (run "let x = 11 in let p = proc (y) -(y,x) in -(setdynamic x = 17 during (p 22), (p 13))"))

