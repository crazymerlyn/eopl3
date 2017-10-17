(load "type.scm")


(define (test)
  (types-to-external-form (type-of-program (scan&parse "proc(x,y:?,?) if x then x else y"))))

