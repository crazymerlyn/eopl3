(define-datatype
  expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (pair-val
    (first expval?)
    (second expval?))
  (proc-val
    (body expression?)
    (env nameless-environment?))
  (null-val))

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (error "invalid expval -- expval->bool" val))))

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else
           (error "invalid expval -- expval->num" val))))

(define (expval->pair val)
  (cases expval val
         (pair-val (first second) (cons first second))
         (else (error "invalid listval -- expval->list" val))))

(define (list->pairval vals)
  (if (null? vals)
      (null-val)
      (pair-val (car vals) (list->pairval (cdr vals)))))


(define (expval-is-null? val)
  (cases expval val
         (null-val () #t)
         (else #f)))



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


(define-datatype
  senvironment senvironment?
  (empty-senv)
  (extend-senv
    (var identifier?)
    (senvironment senvironment?))
  (extend-senv-rec*
    (vars (list-of identifier?))
    (senvironment senvironment?)))
(define (extend-senv* vars senv)
  (if (null? vars)
      senv
      (extend-senv (car vars)
                  (extend-senv* (cdr vars) senv))))

(define (apply-senv senv var)
  (cases
    senvironment senv
    (empty-senv () (error "Unbound value"))
    (extend-senv
      (saved-var saved-senv)
      (if (eq? var saved-var)
          0
          (+ 1 (apply-senv saved-senv var))))
    (extend-senv-rec*
      (vars saved-env)
      (if (memq var vars)
          (list-index var vars)
          (+ (length vars) (apply-senv saved-env var))))))

(define (senv-is-letrecced? senv var)
  (cases
    senvironment senv
    (empty-senv () (error "Unbound value"))
    (extend-senv
      (saved-var saved-senv)
      (if (eq? var saved-var)
          #f
          (senv-is-letrecced? saved-senv var)))
    (extend-senv-rec*
      (vars saved-env)
      (if (memq var vars)
          #t
          (senv-is-letrecced? saved-env var)))))

(define (list-index var vars)
  (if (null? vars)
      (error "Not found" var)
      (if (eq? var (car vars))
          0
          (+ 1 (list-index var (cdr vars))))))


(define-datatype
  nameless-environment nameless-environment?
  (empty-nameless-env)
  (extend-namesless-env
    (val expval?)
    (env nameless-environment?))
  (extend-namesless-env-rec
    (proc-bodies (list-of expression?))
    (env nameless-environment?)))

(define (extend-namesless-env* vals env)
  (if (null? vals)
      env
      (extend-namesless-env (car vals)
                            (extend-namesless-env* (cdr vals) env))))
(define (apply-namesless-env env n)
  (cases
    nameless-environment env
    (empty-nameless-env () (error "Unbound value"))
    (extend-namesless-env (val saved-env)
      (if (= n 0) val (apply-namesless-env saved-env (- n 1))))
    (extend-namesless-env-rec
      (proc-bodies saved-env)
      (if (< n (length proc-bodies))
          (let ((body (list-ref proc-bodies n)))
           (proc-val body env))
          (apply-namesless-env saved-env (- n (length proc-bodies)))))))

