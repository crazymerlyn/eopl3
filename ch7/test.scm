(load "type.scm")


(define (test)
  (types-to-external-form (type-of-program (scan&parse "module m interface
                                                        [opaque t x : t f : (t -> bool)] body [type t = int x = 4 f = proc (x:int) zero?(x)] (from m take f from m take x)"))))

