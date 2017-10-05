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
    (vars list?)
    (body expression?)
    (env environment?))
  (null-val))

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (error "invalid expval -- expval->bool" val))))

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (error "invalid expval -- expval->num" val))))

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


