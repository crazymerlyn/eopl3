(load "type.scm")


(define (test)
  (types-to-external-form (type-of-program (scan&parse "let f = proc(x,y:?,?) if x then 1 else y in (f zero?(1) 1)"))))

