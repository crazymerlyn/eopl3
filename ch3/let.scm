(load "lang.scm")
(load "data-structures.scm")

(define the-global-environment (empty-nameless-env))

(define (value-of exp env)
  (cases
    expression exp
    (const-exp (num) (num-val num))
    (nameless-var-exp (n) (apply-namesless-env env n))
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
    (nameless-let-exp (vals body)
      (value-of body
                (extend-namesless-env*
                  (map (lambda (exp1) (value-of exp1 env))
                        vals)
                  env)))
    (nameless-letrec-exp (proc-bodies letrec-body)
      (value-of letrec-body
                (extend-namesless-env-rec proc-bodies env)))
    (nameless-proc-exp (body)
      (proc-val body env))
    (call-exp (rator rands)
      (apply-procedure
        (value-of rator env)
        (map (lambda (rand) (value-of rand env))
             rands)))
    (else (error "Invalid translated expression" exp))))

(define (apply-procedure proc vals)
  (cases expval proc
         (proc-val (body env)
                   (value-of body (extend-namesless-env* vals env)))
         (else (error "Invalid expval -- apply-procedure" proc))))

(define (value-of-program pgm)
  (cases program pgm
         (a-program (exp1) (value-of exp1 the-global-environment))))

(define (translation-of-program pgm)
  (cases program pgm
         (a-program (exp1)
           (a-program
             (translation-of exp1 (empty-senv))))))

(define (translation-of exp senv)
  (cases
    expression exp
    (const-exp (num) (const-exp num))
    (var-exp (var) (nameless-var-exp (apply-senv senv var)))
    (diff-exp (exp1 exp2)
      (diff-exp (translation-of exp1 senv)
                (translation-of exp2 senv)))
    (list-exp (exps)
              (list-exp
                (map (lambda (exp1) (translation-of exp1 senv))
                     exps)))
    (cons-exp (exp1 exp2)
      (cons-exp (translation-of exp1 senv)
                (translation-of exp2 senv)))
    (car-exp (exp1)
      (car-exp (translation-of exp1 senv)))
    (cdr-exp (exp1)
      (cdr-exp (translation-of exp1 senv)))
    (null?-exp (exp1)
      (null?-exp (translation-of exp1 senv)))
    (emptylist-exp () (emptylist-exp))
    (zero?-exp (exp1)
      (zero?-exp (translation-of exp1 senv)))
    (if-exp (exp1 exp2 exp3)
      (if-exp (translation-of exp1 senv)
              (translation-of exp2 senv)
              (translation-of exp3 senv)))
    (let-exp (vars vals body)
      (nameless-let-exp
        (map (lambda (exp1) (translation-of exp1 senv)) vals)
        (translation-of body
                        (extend-senv* vars senv))))
    (letrec-exp (proc-names vars proc-bodies letrec-body)
      (nameless-letrec-exp
        (map (lambda (vars proc-body)
               (translation-of proc-body
                               (extend-senv* vars
                                             (extend-senv-rec* proc-names senv))))
             vars
             proc-bodies)
        (translation-of letrec-body
                        (extend-senv-rec* proc-names senv))))
    (proc-exp (vars body)
      (nameless-proc-exp
        (translation-of body
                        (extend-senv* vars senv))))
    (call-exp (rator rands)
      (call-exp
        (translation-of rator senv)
        (map (lambda (rand) (translation-of rand senv))
             rands)))
    (else (error "Invalid source expression"))))

(define (run str)
  (value-of-program
    (translation-of-program
      (scan&parse str))))
