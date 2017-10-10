(load "let.scm")


(define (test)
  (run "let f = proc (n) if zero?(n) then 2 else raise 3 in try (f 1) catch (x) x"))

