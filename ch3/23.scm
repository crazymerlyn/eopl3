(load "let.scm")

(display (run "let maketimes = proc (maker) proc (x, y) if zero?(y) then 0 else
               -(((maker maker) x -(y,1)), -(0,x)) in
               let times = proc (x, y) ((maketimes maketimes) x y) in
               let makefact = proc (maker)
                              proc (x)
                               if zero?(x)
                               then 1
                               else (times ((maker maker) -(x, 1)) x) in
              let fact = proc (x) ((makefact makefact) x) in
              (fact 5)"))
(newline)

