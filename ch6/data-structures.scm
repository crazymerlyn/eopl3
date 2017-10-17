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
    (env environment?)))

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



(define (init-tenv) '())
(define (extend-tenv* vars vals tenv)
  (append (map cons vars vals) tenv))
(define (apply-tenv tenv var)
  (cond ((assoc var tenv) => cdr)
        (else (error "Unbound variable"))))

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
