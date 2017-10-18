(load "let.scm")


(define (test)
  (run "class c extends object
        field x
        method initialize (y)
        set x = y
        method get ()
        x

        class c2 extends c
        field y
        method initialize (a, b)
        begin {
          super initialize (a);
          set y = b
        }

        method get ()
        list (x, y)
        
        let o = new c2(1, 2) in
        send o get ()"))

