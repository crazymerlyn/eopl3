(load "lang.scm")
(load "data-structures.scm")

(define (value-of exp env)
  (cases
    expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (deref (apply-env env var)))
    (assign-exp (var exp1)
      (begin
        (setref!
          (apply-env env var)
          (value-of exp1 env))
        (num-val 27)))
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
    (self-exp () (apply-env env '%self))
    (method-call-exp (obj-exp method-name rands)
      (let ((args (values-of-exps rands env))
            (obj (value-of obj-exp env)))
        (apply-method
          (find-method
            (object->class-name obj)
            method-name)
          obj
          args)))
    (super-call-exp (method-name rands)
      (let ((args (values-of-exps rands env))
            (obj (deref (apply-env env '%self))))
        (apply-method
          (find-method (deref (apply-env env '%super)) method-name)
          obj
          args)))
    (new-object-exp (class-name rands)
      (let ((args (values-of-exps rands env))
            (obj (new-object class-name)))
        (apply-method
          (find-method class-name 'initialize)
          obj
          args)
        obj))
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
    (proc-exp (vars body)
      (proc-val vars body env))
    (begin-exp (exps)
      (cond ((null? exps) (num-val 23))
            ((null? (cdr exps)) (value-of (car exps) env))
            (else (value-of (car exps) env)
                  (value-of (begin-exp (cdr exps)) env))))
    (call-exp (rator rands)
      (apply-procedure
        (value-of rator env)
        (map (lambda (rand) (value-of rand env))
             rands)))))

(define (values-of-exps exps env)
  (map (lambda (exp) (value-of exp env))
       exps))

(define (apply-procedure proc vals)
  (cases expval proc
         (proc-val (vars body env)
                   (value-of body (extend-env* vars (map newref vals) env)))
         (else (error "Invalid expval -- apply-procedure" proc))))

(define (apply-method m self args)
  (cases method m
    (a-method (vars body super-name field-names)
      (value-of
        body
        (extend-env*
          vars (map newref args)
          (extend-env-with-self-and-super
            self super-name
            (extend-env* field-names (object->fields self) (empty-env))))))))

(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
    (a-program (class-decls exp1)
      (initialize-class-env! class-decls)
      (value-of exp1 (empty-env)))))

(define (run str)
  (value-of-program (scan&parse str)))
