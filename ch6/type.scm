(load "lang.scm")
(load "data-structures.scm")

(define (check-equal-type! ty1 ty2 exp)
  (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))

(define (report-unequal-types ty1 ty2 exp)
  (error "Types didn't match: ~s != ~a in~%~a"
         (types-to-external-form ty1)
         (types-to-external-form ty2)
         exp))

(define (types-to-external-form ty)
  (cases type ty
         (int-type () 'int)
         (bool-type () 'bool)
         (proc-type (arg-types result-type)
           (list
             (map types-to-external-form arg-type)
             '->
             (types-to-external-form result-type)))))

(define (type-of exp tenv)
  (cases expression exp
         (const-exp (num) (int-type))
         (var-exp (var) (apply-tenv tenv var))
         (diff-exp (exp1 exp2)
           (let ((ty1 (type-of exp1 tenv))
                 (ty2 (type-of exp2 tenv)))
             (check-equal-type! ty1 (int-type) exp1)
             (check-equal-type! ty2 (int-type) exp2)
             (int-type)))
         (zero?-exp (exp1)
           (let ((ty1 (type-of exp1 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (bool-type)))
         (if-exp (exp1 exp2 exp3)
           (let ((ty1 (type-of exp1 tenv))
                 (ty2 (type-of exp2 tenv))
                 (ty3 (type-of exp3 tenv)))
             (check-equal-type! ty1 (bool-type) exp1)
             (check-equal-type! ty2 ty3 exp)
             ty2))
         (let-exp (vars exps body)
           (let ((types (map (lambda (exp1) (type-of exp1 tenv)) exps)))
            (type-of body (extend-tenv* vars types tenv))))
         (proc-exp (vars var-types body)
           (let ((result-type (type-of body (extend-tenv* vars var-types tenv))))
            (proc-type var-types result-type)))
         (call-exp (rator rands)
           (let ((rator-type (type-of rator tenv))
                 (rand-types (map (lambda (rand) (type-of rand tenv))
                                  rands)))
             (cases type rator-type
                    (proc-type (arg-types result-type)
                      (begin
                        (check-equal-types! arg-types rand-types rands)
                        result-type))
                    (else
                      (error "rator not a proc type" rator-type rator)))))))

(define (type-of-program pgm)
  (cases program pgm
         (a-program (exp1) (type-of exp1 (init-tenv)))))

