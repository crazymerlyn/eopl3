(load "lang.scm")
(load "data-structures.scm")

(define (check-equal-type! ty1 ty2 exp)
  (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))

(define (check-equal-types! tys1 tys2 exps)
  (cond ((and (null? tys1) (null? tys2)) 'nothing)
        ((or (null? tys1) (null? tys2)) (error "Wrong number of arguments"))
        (else
          (begin
            (check-equal-type! (car tys1) (car tys2) (car exps))
            (check-equal-types! (cdr tys1) (cdr tys2) (cdr exps))))))

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
             (map types-to-external-form arg-types)
             '->
             (types-to-external-form result-type)))
         (tvar-type (sn)
           (string->symbol
             (string-append
               "ty" (number->string sn))))))

(define (type-of exp tenv subst)
  (cases expression exp
         (const-exp (num) (an-answer (int-type) subst))
         (var-exp (var) (an-answer (apply-tenv tenv var) subst))
         (diff-exp (exp1 exp2)
           (cases answer (type-of exp1 tenv subst)
             (an-answer (ty1 subst1)
               (let ((subst1
                       (unifier ty1 (int-type) subst1 exp1)))
                 (cases answer (type-of exp2 tenv subst1)
                   (an-answer (ty2 subst2)
                     (let ((subst2
                             (unifier ty2 (int-type) subst2 exp2)))
                       (an-answer (int-type) subst2))))))))
         (zero?-exp (exp1)
           (cases answer (type-of exp1 tenv subst)
             (an-answer (ty1 subst1)
               (let ((subst2 (unifier ty1 (int-type) subst1 exp)))
                (an-answer (bool-type) subst2)))))
         (if-exp (exp1 exp2 exp3)
           (cases answer (type-of exp1 tenv subst)
             (an-answer (ty1 subst)
               (let ((subst (unifier ty1 (bool-type) subst exp1)))
                (cases answer (type-of exp2 tenv subst)
                  (an-answer (ty2 subst)
                    (cases answer (type-of exp3 tenv subst)
                      (an-answer (ty3 subst)
                        (let ((subst (unifier ty2 ty3 subst exp)))
                         (an-answer ty2 subst))))))))))
         (let-exp (vars exps body)
           (type-of-seq exps tenv subst
             (lambda (exp-types subst)
               (type-of
                 body
                 (extend-tenv* vars exp-types tenv)
                 subst))))
         (proc-exp (vars var-otypes body)
           (let ((var-types (map otype->type var-otypes)))
            (cases answer (type-of body
                                   (extend-tenv* vars var-types tenv)
                                   subst)
              (an-answer (body-type subst)
                (an-answer
                  (proc-type var-types body-type)
                  subst)))))
         (call-exp (rator rands)
           (let ((result-type (fresh-tvar-type)))
            (cases answer (type-of rator tenv subst)
              (an-answer (rator-type subst)
                (type-of-seq rands tenv subst
                  (lambda (rand-types subst)
                    (let ((subst (unifier
                                   rator-type
                                   (proc-type
                                     rand-types result-type)
                                   subst exp)))
                      (an-answer result-type subst))))))))
         (letrec-exp (p-result-otypes p-names b-vars
                                     b-var-otypes p-bodies letrec-body)
           (let ((p-result-types (map otype->type p-result-otypes))
                 (b-var-types (map (lambda (tys) (map otype->type tys)) b-var-otypes)))
             (let ((tenv-for-letrec-body
                     (extend-tenv*
                       p-names
                       (map proc-type b-var-types p-result-types)
                       tenv)))
               (type-of-letrec-exp p-bodies b-vars p-var-types tenv-for-letrec-body subst
                 (lambda (p-body-types subst)
                   (let ((subst ((unifier-map p-body-types p-result-types
                                             subst p-bodies))))
                     (type-of letrec-body
                              tenv-for-letrec-body
                              subst)))))))))

(define (type-of-letrec-exp bodies vars tys tenv subst func)
  (if (null? bodies) (func '() subst)
      (cases answer (type-of (car bodies) (extend-tenv* (car vars) (car tys) tenv))
             (an-answer (body-type subst)
               (type-of-letrec-exp (cdr bodies) (cdr vars) (cdr tys) tenv subst
                 (lambda (body-types subst)
                   (func (cons body-type body-types) subst)))))))

(define (type-of-seq exps tenv subst func)
  (if (null? exps) (func '() subst)
      (cases answer (type-of (car exps) tenv subst)
        (an-answer (ty subst)
         (type-of-seq
           (cdr exps) tenv subst
           (lambda (tys subst)
             (func (cons ty tys) subst)))))))

(define (type-of-program pgm)
  (cases program pgm
         (a-program (exp1)
           (cases answer (type-of exp1 (init-tenv) (empty-subst))
                  (an-answer (ty subst)
                    (apply-subst-to-type ty subst))))))


(define (apply-one-subst ty0 tvar ty1)
  (cases type ty0
         (int-type () (int-type))
         (bool-type () (bool-type))
         (proc-type (arg-types result-type)
           (proc-type
             (map (lambda (arg-type) (apply-one-subst arg-type tvar ty1))
                  arg-types)
             (apply-one-subst result-type tvar ty1)))
         (tvar-type (sn) (if (equal? ty0 tvar) ty1 ty0))))

(define (apply-subst-to-type ty subst)
  (cases type ty
         (int-type () (int-type))
         (bool-type () (bool-type))
         (proc-type (arg-types result-type)
           (proc-type
             (map (lambda (t1) (apply-subst-to-type t1 subst))
                  arg-types)
             (apply-subst-to-type result-type subst)))
         (tvar-type (sn)
           (let ((tmp (assoc ty subst)))
            (if tmp (cdr tmp) ty)))))

(define (unifier ty1 ty2 subst exp)
  (let ((ty1 (apply-subst-to-type ty1 subst))
        (ty2 (apply-subst-to-type ty2 subst)))
    (cond ((equal? ty1 ty2) subst)
          ((tvar-type? ty1)
           (if (no-occurrence? ty1 ty2)
               (extend-subst subst ty1 ty2)
               (error "No occurence rule violated" ty1 ty2 exp)))
          ((tvar-type? ty2) (unifier ty2 ty1 subst exp))
          ((and (proc-type? ty1) (proc-type? ty2))
           (let ((subst (unifier-map
                          (proc-type->arg-types ty1)
                          (proc-type->arg-types ty2)
                          subst exp)))
             (unifier
               (proc-type->result-type ty1)
               (proc-type->result-type ty2)
               subst exp)))
          (else (error "Can't unify" ty1 ty2 exp)))))

(define (unifier-map tys1 tys2 subst exp)
  (if (null? tys1)
      #t
      (and (unifier (car tys1) (car tys2) subst exp)
           (unifier-map (cdr tys1) (cdr tys2) subst exp))))

(define (no-occurrence? tvar ty)
  (cases type ty
         (int-type () #t)
         (bool-type () #t)
         (proc-type (arg-types result-type)
           (and
             (no-occurrence?-map tvar arg-types)
             (no-occurrence? tvar result-type)))
         (tvar-type (sn) (not (equal? tvar ty)))))

(define (no-occurrence?-map tvar tys)
  (not (any (map (lambda (ty) (no-occurrence? tvar ty)) tys))))
