(load "lang.scm")
(load "data-structures.scm")

(define the-global-environment (empty-env))

(define (value-of exp env)
  (cases
    expression exp
    (const-exp (num) (num-val num))
    (var-exp (var)
      (let ((w (deref (apply-env env var))))
        (if (expval? w) w (value-of-thunk w))))
    (diff-exp (exp1 exp2)
      (num-val
        (-
          (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (list-exp (exps) (list->pairval
                       (map (lambda (ex) (value-of ex env)) exps)))
    (cons-exp (exp1 exp2)
      (pair-val (make-pair (value-of exp1 env)
                           (value-of exp2 env))))
    (car-exp (exp1)
      (left (expval->mutpair (value-of exp1 env))))
    (cdr-exp (exp1)
      (right (expval->mutpair (value-of exp1 env))))
    (set-car-exp (exp1 exp2)
      (let ((val1 (value-of exp1 env))
            (val2 (value-of exp2 env)))
        (begin
          (setleft (expval->mutpair val1) val2)
          (num-val 82))))
    (set-cdr-exp (exp1 exp2)
      (let ((val1 (value-of exp1 env))
            (val2 (value-of exp2 env)))
        (begin
          (setright (expval->mutpair val1) val2)
          (num-val 83))))
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
    (dynamic-exp (var exp1 exp2)
      (let ((original (value-of (var-exp var) env)))
       (begin
         (setref! (apply-env env var) (value-of exp1 env))
         (let ((ans (value-of exp2 env)))
          (setref! (apply-env env var) original)
          ans))))
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
        (map (lambda (rand) (value-of-operand rand env))
             rands)))))

(define (value-of-operand rand env)
  (cases expression rand
         (var-exp (var) (apply-env env var))
         (else (newref (a-thunk rand env)))))

(define (apply-procedure proc vals)
  (cases expval proc
         (proc-val (vars body env)
                   (value-of body (extend-env* vars vals env)))
         (else (error "Invalid expval -- apply-procedure" proc))))

(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
         (a-program (exp1) (value-of exp1 the-global-environment))))

(define (run str)
  (value-of-program (scan&parse str)))
