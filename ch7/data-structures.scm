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
               (else (apply-env saved-env var))))
         (extend-env-with-module (m-name m-val saved-env)
           (apply-env saved-env var))))

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
    (saved-tenv typed-environment?))
  (extend-tenv-with-type
    (name symbol?)
    (type type?)
    (saved-tenv typed-environment?)))

(define (extend-tenv* vars vals tenv)
  (let ((vals (map (lambda (val) (expand-type val tenv)) vals)))
   (if (null? vars)
       tenv
       (extend-tenv
         (car vars) (car vals)
         (extend-tenv* (cdr vars) (cdr vals) tenv)))))

(define (apply-tenv tenv var)
  (cases typed-environment tenv
    (init-tenv () (error "Unbound variable var"))
    (extend-tenv (saved-var val saved-tenv)
      (if (equal? var saved-var) val (apply-tenv saved-tenv var)))
    (extend-tenv-with-module (name interface saved-tenv)
      (apply-tenv saved-tenv var))
    (extend-tenv-with-type (name ty saved-tenv)
      (apply-tenv saved-tenv var))))

(define (lookup-qualified-var-in-tenv m-name var-name tenv)
  (let ((iface (lookup-module-name-in-tenv tenv m-name)))
   (cases interface iface
     (simple-iface (decls)
       (lookup-variable-name-decls var-name decls))
     (proc-iface (a b c) (error "Can't lookup values in proc-iface")))))

(define (lookup-variable-name-decls var-name decls)
  (cond ((null? decls)
         (error "Variable not exported in module" var-name))
        (else
          (cases decl (car decls)
            (val-decl (name var-type)
              (if (eqv? name var-name)
                  var-type
                  (lookup-variable-name-decls var-name (cdr decls))))
            (else (lookup-variable-name-decls var-name (cdr decls)))))))

(define (lookup-qualified-type-in-tenv m-name type-name tenv)
  (let ((iface (lookup-module-name-in-tenv tenv m-name)))
   (cases interface iface
     (simple-iface (decls)
       (lookup-type-name-decls type-name decls))
     (proc-iface (a b c) (error "Can't lookup types in proc-iface")))))


(define (lookup-type-name-decls type-name decls)
  (cond ((null? decls)
         (error "Type not exported in module" type-name))
        (else
          (cases decl (car decls)
            (transparent-type-decl (name type)
              (if (eqv? name type-name)
                  type
                  (lookup-type-name-decls type-name (cdr decls))))
            (else (lookup-type-name-decls type-name (cdr decls)))))))

(define (lookup-module-name-in-tenv tenv m-name)
  (cases typed-environment tenv
    (init-tenv () (error "Module not found" m-name))
    (extend-tenv (var val saved-tenv)
      (lookup-module-name-in-tenv saved-tenv m-name))
    (extend-tenv-with-module (name interface saved-tenv)
      (if (eqv? name m-name)
          interface
          (lookup-module-name-in-tenv saved-tenv m-name)))
    (extend-tenv-with-type (name ty saved-tenv)
      (lookup-module-name-in-tenv saved-tenv m-name))))

(define (lookup-type-name-in-tenv tenv name)
  (cases typed-environment tenv
    (init-tenv () (error "Type not found" name))
    (extend-tenv (var val saved-tenv)
      (lookup-type-name-in-tenv saved-tenv name))
    (extend-tenv-with-module (name interface saved-tenv)
      (lookup-type-name-in-tenv saved-tenv name))
    (extend-tenv-with-type (saved-name ty saved-tenv)
      (if (eqv? saved-name name)
          ty
          (lookup-type-name-in-tenv saved-tenv name)))))

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
                         (expand-iface m-name iface tenv)
                         tenv)))
                 (add-module-defns-to-tenv
                   (cdr defns) new-tenv))
               (begin
                 (display iface)
                 (newline)
                 (display actual-iface)
                 (newline)
                 (error "Module doesn't satisfy interface" m-name iface actual-iface))))))))

(define (expand-iface m-name iface tenv)
  (cases interface iface
    (simple-iface (decls)
      (simple-iface
        (expand-decls m-name decls tenv)))
    (proc-iface (param-name param-iface result-iface)
      iface)))

(define (expand-decls m-name decls tenv)
  (if (null? decls) '()
      (cases decl (car decls)
        (opaque-type-decl (t-name)
          (let ((expanded-type (qualified-type m-name t-name)))
           (let ((new-tenv (extend-tenv-with-type t-name expanded-type tenv)))
            (cons (transparent-type-decl t-name expanded-type)
                  (expand-decls m-name (cdr decls) new-tenv)))))
        (transparent-type-decl (t-name ty)
          (let ((expanded-type (expand-type ty tenv)))
           (let ((new-tenv (extend-tenv-with-type t-name expanded-type tenv)))
            (cons (transparent-type-decl t-name expanded-type)
                  (expand-decls m-name (cdr decls) new-tenv)))))
        (val-decl (var-name ty)
          (let ((expanded-type (expand-type ty tenv)))
           (cons (val-decl var-name expanded-type)
                 (expand-decls m-name (cdr decls) tenv)))))))

(define (interface-of m-body tenv)
  (cases module-body m-body
    (var-module-body (m-name)
      (lookup-module-name-in-tenv tenv m-name))
    (defns-module-body (defns)
      (simple-iface (defns-to-decls defns tenv)))
    (app-module-body (rator-id rand-id)
      (let ((rator-iface (lookup-module-name-in-tenv tenv rator-id))
            (rand-iface (lookup-module-name-in-tenv tenv rand-id)))
        (cases interface rator-iface
          (simple-iface (delcs)
            (error "Attempted to apply simple module" rator-id rator-val))
          (proc-iface (param-name param-iface result-iface)
            (if (<:-iface rand-iface param-iface tenv)
                (rename-in-iface result-iface param-name rand-id)
                (error "parameter doesn't implement required iface"
                       rand-iface param-iface))))))
    (proc-module-body (rand-name rand-iface m-body)
      (let ((body-iface
              (interface-of
                m-body
                (extend-tenv-with-module
                  rand-name
                  (expand-iface rand-name rand-iface tenv)
                  tenv))))
        (proc-iface rand-name rand-iface body-iface)))))

(define (rename-in-iface iface name1 name2)
  (cases interface iface
    (simple-iface (decls)
      (simple-iface
        (map (lambda (decl) (rename-in-decl decl name1 name2))
             decls)))
    (proc-iface (param-name param-iface result-iface)
      (proc-iface
        param-name
        (rename-in-iface param-iface name1 name2)
        (rename-in-iface result-iface name1 name2)))))

(define (rename-in-decl d name1 name2)
  (cases decl d
    (val-decl (name ty)
      (val-decl name (rename-in-type ty name1 name2)))
    (transparent-type-decl (name ty)
      (transparent-type-decl
        name (rename-in-type ty name1 name2)))
    (else d)))

(define (rename-in-type ty name1 name2)
  (cases type ty
    (qualified-type (m-name t-name)
      (qualified-type
        (if (eqv? m-name name1) name2 m-name)
        t-name))
    (else ty)))

(define (defns-to-decls defns tenv)
  (if (null? defns)
      '()
      (cases definition (car defns)
        (val-defn (var-name exp)
          (let ((ans (type-of exp tenv (empty-subst))))
            (cases answer ans
              (an-answer (ty subst)
                (cons (val-decl var-name (apply-subst-to-type ty subst))
                      (defns-to-decls
                        (cdr defns)
                        (extend-tenv var-name ty tenv)))))))
        (type-defn (name ty)
          (let ((new-env
                  (extend-tenv-with-type
                    name (expand-type ty tenv) tenv)))
            (cons (transparent-type-decl name ty)
                  (defns-to-decls (cdr defns) new-env)))))))

(define (<:-iface iface1 iface2 tenv)
  (cases interface iface1
    (simple-iface (decls1)
      (cases interface iface2
        (simple-iface (decls2)
          (<:-decls decls1 decls2 tenv))
        (proc-iface (param-name2 param-iface2 result-iface2)
          #t)))
    (proc-iface (param-name1 param-iface1 result-iface1)
      (cases interface iface2
        (simple-iface (decls2) #f)
        (proc-iface (param-name2 param-iface2 result-iface2)
          (let ((new-name (fresh-module-name param-name1)))
           (let ((result-iface1 (rename-in-iface
                                  result-iface1 param-name1 new-name))
                 (result-iface2 (rename-in-iface
                                  result-iface2 param-name2 new-name)))
             (and
               (<:-iface param-iface2 param-iface1 tenv)
               (<:-iface result-iface1 result-iface2
                 (extend-tenv-with-module
                   new-name
                   (expand-iface new-name param-iface1 tenv)
                   tenv))))))))))

(define (<:-decls decls1 decls2 tenv)
  (cond ((null? decls2) #t)
        ((null? decls1) #f)
        (else
          (let ((name1 (decl->name (car decls1)))
                (name2 (decl->name (car decls2))))
            (if (eqv? name1 name2)
                (and (<:-decl (car decls1) (car decls2) tenv)
                     (<:-decls (cdr decls1) (cdr decls2)
                               (extend-tenv-with-decl (car decls1) tenv)))
                (<:-decls (cdr decls1) decls2
                          (extend-tenv-with-decl (car decls1) tenv)))))))

(define (<:-decl decl1 decl2 tenv)
  (or
    (and (val-decl? decl1)
         (val-decl? decl2)
         (equiv-type?
           (decl->type decl1)
           (decl->type decl2) tenv))
    (and (transparent-type-decl? decl1)
         (transparent-type-decl? decl2)
         (equiv-type?
           (decl->type decl1)
           (decl->type decl2) tenv))
    (and (transparent-type-decl? decl1)
         (opaque-type-decl? decl2))
    (and (opaque-type-decl? decl1)
         (opaque-type-decl? decl2))))

(define (equiv-type? ty1 ty2 tenv)
  (equal? (expand-type ty1 tenv)
          (expand-type ty2 tenv)))

(define (extend-tenv-with-decl d tenv)
  (cases decl d
    (val-decl (name ty) tenv)
    (transparent-type-decl (name ty)
      (extend-tenv-with-type
        name (expand-type ty tenv) tenv))
    (opaque-type-decl (ty-name)
      (extend-tenv-with-type
        ty-name
        (qualified-type (fresh-module-name '%unknown) ty-name)
        tenv))))


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

(define fresh-module-name
  (let ((sn 0))
   (lambda (name)
     (set! sn (+ sn 1))
     (string->symbol
       (string-append
         (symbol->string
           name)
         (number->string sn))))))

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
    (bindings environment?))
  (proc-module
    (b-var symbol?)
    (body module-body?)
    (saved-env environment?)))

(define (lookup-qualified-var-in-env m-name var-name env)
  (let ((m-val (lookup-module-name-in-env env m-name)))
   (cases typed-module m-val
          (simple-module (bindings) (apply-env bindings var-name))
          (proc-module (a b c) (error "Can't lookup values in proc-module" m-val)))))

(define (lookup-module-name-in-env env m-name)
  (cases environment env
    (empty-env () (error "Module not found" m-name))
    (extend-env (var val saved-env)
      (lookup-module-name-in-env saved-env m-name))
    (extend-env-rec (names vars bodies saved-env)
      (lookup-module-name-in-env saved-env m-name))
    (extend-env-with-module (name interface saved-env)
      (if (eqv? name m-name)
          interface
          (lookup-module-name-in-env saved-env m-name)))))

(define (val-decl? d)
  (cases decl d
         (val-decl (x y) #t)
         (else #f)))

(define (transparent-type-decl? d)
  (cases decl d
         (transparent-type-decl (x y) #t)
         (else #f)))

(define (opaque-type-decl? d)
  (cases decl d
         (opaque-type-decl (y) #t)
         (else #f)))

(define (decl->name d)
  (cases decl d
    (val-decl (var-name x) var-name)
    (transparent-type-decl (ty-name x) ty-name)
    (opaque-type-decl (ty-name) ty-name)))

(define (decl->type d)
  (cases decl d
    (val-decl (v var-type) var-type)
    (transparent-type-decl (v ty) ty)
    (opaque-type-decl (ty-name) (error "Can't convert opaque ty" ty-name))))

(define (expand-type ty tenv)
  (cases type ty
    (int-type () (int-type))
    (bool-type () (bool-type))
    (proc-type (arg-types result-type)
      (proc-type
        (map (lambda (arg-type) (expand-type arg-type tenv))
             arg-types)
        (expand-type result-type tenv)))
    (named-type (name)
      (lookup-type-name-in-tenv tenv name))
    (qualified-type (m-name t-name)
      (lookup-qualified-type-in-tenv m-name t-name tenv))
    (else ty)))
