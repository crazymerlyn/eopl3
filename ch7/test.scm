(load "let.scm")


(define (test)
  (run "module m interface
        ((ints : [opaque t x:t f:(t -> bool)]) => [y:bool])
        body
        module-proc (ints: [opaque t x:t f:(t -> bool)])
        [y = (from ints take f from ints take x)]

        module n interface [opaque t x:t f:(t -> bool)]
        body [type t = int x = 4 f = proc(y : int) zero?(y)]

        module o interface [y:bool]
        body (m n)
        
        from o take y"))

