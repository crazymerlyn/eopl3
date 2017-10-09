(define-datatype
  mutpair mutpair?
  (a-pair
    (left-loc reference?)
    (right-loc reference?)))

(define (make-pair val1 val2)
  (a-pair (newref val1) (newref val2)))

(define (left p)
  (cases mutpair p
         (a-pair (left-loc right-loc)
           (deref left-loc))))

(define (right p)
  (cases mutpair p
         (a-pair (left-loc right-loc)
           (deref right-loc))))

(define (setleft p val)
  (cases mutpair p
         (a-pair (left-loc right-loc)
           (setref! left-loc val))))

(define (setright p val)
  (cases mutpair p
         (a-pair (left-loc right-loc)
           (setref! right-loc val))))

(define-datatype
  expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (ref-val
    (ref integer?))
  (pair-val
    (val mutpair?))
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

(define (expval->ref val)
  (cases expval val
         (ref-val (ref) ref)
         (else (error "invalid expval -- expval->ref" val))))

(define (expval->mutpair val)
  (cases expval val
         (pair-val (val) val)
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
    (val reference?)
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
                  => (lambda (struct) (newref (proc-val (cadr struct) (caddr struct) env))))
               (else (apply-env saved-env var))))))

(define (extend-env* vars vals env)
  (cond ((and (null? vars) (null? vals)) env)
        ((or (null? vars) (null? vals)) (error "Invalid no. of arguments"))
        (else (extend-env
                (car vars) (car vals)
                (extend-env* (cdr vars) (cdr vals) env)))))


(define (empty-store) '())
(define the-store 'uninitialized)
(define (initialize-store!)
  (set! the-store (empty-store)))
(define (reference? v) (integer? v))
(define (newref val)
  (let ((next-ref (length the-store)))
   (set! the-store (append the-store (list val)))
   next-ref))
(define (deref ref)
  (list-ref the-store ref))
(define (setref! ref val)
  (set! the-store
    (letrec
      ((setref-inner
         (lambda (store1 ref1)
           (cond
             ((null? store1) (error "Invalid refernce" ref store1))
             ((zero? ref1) (cons val (cdr store1)))
             (else (cons (car store1)
                         (setref-inner
                           (cdr store1) (- ref1 1))))))))
      (setref-inner the-store ref))))

