(load "lang.scm")
(load "data-structures.scm")
(load "type.scm")

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
    (let-exp (vars vals body)
      (value-of body
                (extend-env* vars
                            (map (lambda (exp1) (value-of exp1 env))
                                 vals)
                            env)))
    (letrec-exp (proc-names vars proc-bodies letrec-body)
      (value-of letrec-body
                (extend-env-rec proc-names vars proc-bodies env)))
    (proc-exp (vars body)
      (proc-val vars body env))
    (call-exp (rator rands)
      (apply-procedure
        (value-of rator env)
        (map (lambda (rand) (value-of rand env))
             rands)))))

(define (apply-procedure proc vals)
  (cases expval proc
         (proc-val (vars body env)
                   (value-of body (extend-env* vars vals env)))
         (else (error "Invalid expval -- apply-procedure" proc))))

(define (value-of-program pgm)
  (cases program pgm
         (a-program (exp1) (value-of exp1 the-global-environment))))

(define (run str)
  (let ((pgm (scan&parse str)))
   (type-of-program pgm)
   (value-of-program pgm)))
