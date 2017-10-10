(load "lang.scm")
(load "data-structures.scm")

(define the-global-environment (empty-env))

(define (value-of/k exp env cont)
  (cases
    expression exp
    (const-exp (num) (apply-cont cont (num-val num)))
    (var-exp (var)
      (let ((w (deref (apply-env env var))))
        (if (expval? w)
            (apply-cont cont w)
            (value-of-thunk w cont))))
    (diff-exp (exp1 exp2)
      (value-of/k exp1 env (diff1-cont exp2 env cont)))
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
      (value-of/k exp1 env (zero1-cont cont)))
    (if-exp (exp1 exp2 exp3)
      (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
    (try-exp (exp1 var handler-exp)
      (value-of/k exp1 env
                  (try-cont var handler-exp env cont)))
    (raise-exp (exp1)
      (value-of/k exp1 env (raise1-cont cont)))
    (let-exp (vars vals body)
      (list-value-of/k
        vals
        env
        (let-exp-cont vars body env cont)))
    (letrec-exp (proc-names vars proc-bodies letrec-body)
      (value-of/k letrec-body
                  (extend-env-rec proc-names vars proc-bodies env)
                  cont))
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
      (apply-cont cont (proc-val vars body env)))
    (call-exp (rator rands)
      (value-of/k rator env (rator-cont rands env cont)))))

(define (list-value-of/k exps env cont)
  (apply-cont cont (map (lambda (exp) (value-of-operand exp env)) exps)))

(define (value-of-operand rand env)
  (cases expression rand
         (var-exp (var) (apply-env env var))
         (else (newref (a-thunk rand env)))))

(define (apply-procedure/k proc vals cont)
  (cases expval proc
         (proc-val (vars body env)
                   (lambda ()  (value-of/k body (extend-env* vars vals env) cont)))
         (else (error "Invalid expval -- apply-procedure" proc))))

(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
         (a-program (exp1)
           (trampoline (value-of/k exp1 the-global-environment (end-cont))))))

(define (trampoline bounce)
  (if (expval? bounce) bounce (trampoline (bounce))))

(define (run str)
  (value-of-program (scan&parse str)))
