(define-datatype
  expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (vars list?)
    (body expression?)
    (env environment?)))

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (error "invalid expval -- expval->bool" val))))

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (error "invalid expval -- expval->num" val))))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?))
  (extend-env-rec
    (p-names list?)
    (b-vars list?)
    (bodies list?)
    (env environment?))
  (extend-env-with-module
    (m-name symbol?)
    (m-val typed-module?)
    (saved-env environment?)))

(define (apply-env env var)
  (cases environment env
         (empty-env () (error "No binding for " var))
         (extend-env (saved-var saved-val saved-env)
           (if (eq? var saved-var)
               saved-val
               (apply-env saved-env var)))
         (extend-env-rec (p-names b-vars bodies saved-env)
           (cond ((assoc var (map list p-names b-vars bodies))
                  => (lambda (struct) (proc-val (cadr struct) (caddr struct) env)))
               (else (apply-env saved-env var))))))

(define (extend-env* vars vals env)
  (cond ((and (null? vars) (null? vals)) env)
        ((or (null? vars) (null? vals)) (error "Invalid no. of arguments"))
        (else (extend-env
                (car vars) (car vals)
                (extend-env* (cdr vars) (cdr vals) env)))))


(define-datatype typed-environment typed-environment?
  (init-tenv)
  (extend-tenv
    (var identifier?)
    (val type?)
    (saved-tenv typed-environment?))
  (extend-tenv-with-module
    (name symbol?)
    (interface interface?)
    (saved-tenv typed-environment?)))

(define (extend-tenv* vars vals tenv)
  (if (null? vars)
      tenv
      (extend-tenv (car vars) (car vals)
                   (extend-tenv* (cdr vars) (cdr vals) tenv))))

(define (apply-tenv tenv var)
  (cases typed-environment tenv
    (init-tenv () (error "Unbound variable var"))
    (extend-tenv (saved-var val saved-tenv)
      (if (equal? var saved-var) val (apply-tenv saved-tenv var)))
    (extend-tenv-with-module (name interface saved-tenv)
      (apply-tenv saved-tenv var))))

(define (lookup-qualified-var-in-tenv m-name var-name tenv)
  (let ((iface (lookup-module-name-in-tenv tenv m-name)))
   (cases interface iface
     (simple-iface (decls)
       (lookup-variable-name-decls var-name decls)))))

(define (lookup-variable-name-decls var-name decls)
  (cond ((null? decls)
         (error "Variable not exported in module" var-name))
        (else
          (cases decl (car decls)
            (val-decl (name var-type)
              (if (eqv? name var-name)
                  var-type
                  (lookup-variable-name-decls var-name (cdr decls))))))))

(define (lookup-module-name-in-tenv tenv m-name)
  (cases typed-environment tenv
    (init-tenv () (error "Module not found" m-name))
    (extend-tenv (var val saved-tenv)
      (lookup-module-name-in-tenv saved-tenv m-name))
    (extend-tenv-with-module (name interface saved-tenv)
      (if (eqv? name m-name)
          interface
          (lookup-module-name-in-tenv saved-tenv m-name)))))

(define (add-module-defns-to-tenv defns tenv)
  (if (null? defns)
      tenv
      (cases module-def (car defns)
        (a-module-definition (m-name iface m-body)
          (let ((actual-iface (interface-of m-body tenv)))
           (if (<:-iface actual-iface iface tenv)
               (let ((new-tenv
                       (extend-tenv-with-module
                         m-name
                         iface
                         tenv)))
                 (add-module-defns-to-tenv
                   (cdr defns) new-tenv))
               (error "Module doesn't satisfy interface" m-name iface actual-iface)))))))

(define (interface-of m-body tenv)
  (cases module-body m-body
    (defns-module-body (defns)
      (simple-iface (defns-to-decls defns tenv)))))

(define (defns-to-decls defns tenv)
  (if (null? defns)
      '()
      (cases definition (car defns)
        (val-defn (var-name exp)
          (let ((ans (type-of exp tenv (empty-subst))))
            (cases answer ans
                   (an-answer (ty subst)
                              (cons (val-decl var-name ty)
                                    (defns-to-decls
                                      (cdr defns)
                                      (extend-tenv var-name ty tenv))))))))))

(define (<:-iface iface1 iface2 tenv)
  (cases interface iface1
    (simple-iface (decls1)
      (cases interface iface2
        (simple-iface (decls2)
          (<:-decls decls1 decls2 tenv))))))

(define (<:-decls decls1 decls2 tenv)
  (cond ((null? decls2) #t)
        ((null? decls1) #f)
        (else
          (let ((name1 (decl->name (car decls1)))
                (name2 (decl->name (car decls2))))
            (if (eqv? name1 name2)
                (and (equal? (decl->type (car decls1))
                             (decl->type (car decls2)))
                     (<:-decls (cdr decls1) (cdr decls2) tenv))
                (<:-decls (cdr decls1) decls2 tenv))))))


(define (empty-subst) '())
(define (extend-subst subst tvar ty)
  (cons
    (cons tvar ty)
    (map
      (lambda (p)
        (let ((oldlhs (car p))
              (oldrhs (cdr p)))
          (cons oldlhs (apply-one-subst oldrhs tvar ty))))
      subst)))

(define substitution? list?)

(define (otype->type otype)
  (cases optional-type otype
         (no-type () (fresh-tvar-type))
         (a-type (ty) ty)))

(define fresh-tvar-type
  (let ((sn 0))
   (lambda ()
     (set! sn (+ sn 1))
     (tvar-type sn))))

(define-datatype answer answer?
  (an-answer
    (ty type?)
    (subst substitution?)))

(define (tvar-type? t)
  (cases type t
         (tvar-type (sn) #t)
         (else #f)))

(define (proc-type? t)
  (cases type t
         (proc-type (a b) #t)
         (else #f)))

(define (proc-type->arg-types t)
  (cases type t
         (proc-type (arg-types r) arg-types)
         (else (error "Not a procedure type" t))))

(define (proc-type->result-type t)
  (cases type t
         (proc-type (a r) r)
         (else (error "Not a procedure type" t))))

(define-datatype typed-module typed-module?
  (simple-module
    (bindings environment?)))

(define (lookup-qualified-var-in-env m-name var-name env)
  (let ((m-val (lookup-module-name-in-env m-name env)))
   (cases typed-module m-val
          (simple-module (bindings) (apply-env bindings var-name)))))

(define (lookup-module-name-in-env env m-name)
  (cases typed-environment env
    (init-env () (error "Module not found" m-name))
    (extend-env (var val saved-env)
      (lookup-module-name-in-env saved-env m-name))
    (extend-env-with-module (name interface saved-env)
      (if (eqv? name m-name)
          interface
          (lookup-module-name-in-env saved-env m-name)))))

(define (decl->name d)
  (cases decl d
    (val-decl (var-name x) var-name)))

(define (decl->type d)
  (cases decl d
    (val-decl (v var-type) var-type)))
