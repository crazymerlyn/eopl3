(load "lang.scm")
(load "../ch2/5.scm") ; environment
(load "data-structures.scm")

(define the-global-environment (empty-env))

(define (value-of exp env)
  (cases
    expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (diff-exp (exp1 exp2)
      (num-val
        (-
          (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (zero?-exp (exp1)
      (bool-val (zero? (expval->num (value-of exp1 env)))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))
    (let-exp (var exp1 body)
      (value-of body
                (extend-env var
                            (value-of exp1 env)
                            env)))))

(define (value-of-program pgm)
  (cases program pgm
         (a-program (exp1) (value-of exp1 the-global-environment))))

(define (run str)
  (value-of-program (scan&parse str)))
