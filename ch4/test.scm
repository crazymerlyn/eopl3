(load "let.scm")


(define (test)
  (run "let g = let count = newref(0) in proc () begin { setref(count, -(deref(count), -1)); deref(count)}
        in begin {(g); (g); (g)}"))

