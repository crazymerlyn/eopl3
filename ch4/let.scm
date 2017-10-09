(load "lang.scm")
(load "data-structures.scm")

(define the-global-environment (empty-env))

(define (value-of exp env)
  (cases
    expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (deref (apply-env env var)))
    (diff-exp (exp1 exp2)
      (num-val
        (-
          (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (list-exp (exps) (list->pairval
                       (map (lambda (ex) (value-of ex env)) exps)))
    (cons-exp (exp1 exp2)
      (pair-val (value-of exp1 env)
                (value-of exp2 env)))
    (car-exp (exp1)
      (car (expval->pair (value-of exp1 env))))
    (cdr-exp (exp1)
      (cdr (expval->pair (value-of exp1 env))))
    (null?-exp (exp1)
      (expval-is-null? (value-of exp1 env)))
    (emptylist-exp () (null-val))
    (zero?-exp (exp1)
      (bool-val (zero? (expval->num (value-of exp1 env)))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))
    (let-exp (vars vals body)
      (value-of body
                (extend-env* vars
                            (map (lambda (exp1) (newref (value-of exp1 env)))
                                 vals)
                            env)))
    (letrec-exp (proc-names vars proc-bodies letrec-body)
      (value-of letrec-body
                (extend-env-rec proc-names vars proc-bodies env)))
    (newref-exp (exp1)
      (ref-val (newref (value-of exp1 env))))
    (deref-exp (exp1)
      (deref (expval->ref (value-of exp1 env))))
    (assign-exp (var exp1)
      (begin
        (setref!
          (apply-env env var)
          (value-of exp1 env))
        (num-val 27)))
    (begin-exp (exps)
      (cond ((null? exps) (num-val 23))
            ((null? (cdr exps)) (value-of (car exps) env))
            (else (value-of (car exps) env)
                  (value-of (begin-exp (cdr exps)) env))))
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
                   (value-of body (extend-env* vars (map newref vals) env)))
         (else (error "Invalid expval -- apply-procedure" proc))))

(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
         (a-program (exp1) (value-of exp1 the-global-environment))))

(define (run str)
  (value-of-program (scan&parse str)))
