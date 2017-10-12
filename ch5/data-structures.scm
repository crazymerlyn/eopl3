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


(define-datatype
  thunk thunk?
  (a-thunk
    (exp1 expression?)
    (env environment?)))

(define (value-of-thunk th cont)
  (cases thunk th
         (a-thunk (exp1 env)
           (value-of/k exp1 env cont))))

(define-datatype
  continuation continuation?
  (end-cont)
  (zero1-cont
    (saved-cont continuation?))
  (let-exp-cont
    (vars (list-of identifier?))
    (body expression?)
    (env environment?)
    (saved-cont continuation?))
  (list-value-cont
    (args (list-of expression?))
    (env environment?)
    (saved-cont continuation?))
  (list-append-cont
    (arg expval?)
    (saved-cont continuation?))
  (if-test-cont
    (conseq expression?)
    (alternative expression?)
    (env environment?)
    (saved-cont continuation?))
  (try-cont
    (var identifier?)
    (handler-exp expression?)
    (env environment?)
    (saved-cont continuation?))
  (raise1-cont
    (saved-cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (env environment?)
    (saved-cont continuation?))
  (diff2-cont
    (val1 expval?)
    (saved-cont continuation?))
  (rator-cont
    (rands (list-of expression?))
    (env environment?)
    (saved-cont continuation?))
  (rands-cont
    (proc expval?)
    (saved-cont continuation?))
  (spawn-cont
    (saved-cont continuation?))
  (end-subthread-cont)
  (end-main-thread-cont))

(define (apply-cont cont val)
  (if (time-expired?)
      (begin
        (place-on-ready-queue! (lambda () (apply-cont cont val)))
        (run-next-thread))
      (begin
        (decrement-timer!)
        (cases continuation cont
               (end-cont () (eopl:printf "End of computation") val)
               (zero1-cont (saved-cont)
                           (apply-cont saved-cont
                                       (bool-val (zero? (expval->num val)))))
               (let-exp-cont (vars body env saved-cont)
                             (value-of/k
                               body
                               (extend-env* vars val env) saved-cont))
               (list-value-cont (args env saved-cont)
                                (list-value-of/k args env (list-append-cont (value-of-operand val env) saved-cont)))
               (list-append-cont (arg saved-cont)
                                 (apply-cont saved-cont (cons arg val)))
               (if-test-cont (conseq alt env saved-cont)
                             (if (expval->bool val)
                                 (value-of/k conseq env saved-cont)
                                 (value-of/k alt env saved-cont)))
               (try-cont (var handler-exp env saved-cont)
                         (apply-cont saved-cont val))
               (raise1-cont (saved-cont)
                            (apply-handler val saved-cont))
               (diff1-cont (exp2 env saved-cont)
                           (value-of/k exp2 env (diff2-cont val saved-cont)))
               (diff2-cont (val1 saved-cont)
                           (let ((num1 (expval->num val1))
                                 (num2 (expval->num val)))
                             (apply-cont saved-cont (num-val (- num1 num2)))))
               (rator-cont (rands env saved-cont)
                           (list-value-of/k rands env (rands-cont val saved-cont)))
               (rands-cont (proc saved-cont)
                           (apply-procedure/k proc val saved-cont))
               (spawn-cont (saved-cont)
                           (place-on-ready-queue!
                             (lambda ()
                               (apply-procedure/k
                                 proc1 '() (end-subthread-cont))))
                           (apply-cont saved-cont (num-val 73)))
               (end-main-thread-cont ()
                                     (set-final-answer! val)
                                     (run-next-thread))
               (end-subthread-cont ()
                                   (run-next-thread))))))

(define (apply-handler val cont)
  (cases continuation cont
         (try-cont (var handler-exp saved-env saved-cont)
           (value-of/k handler-exp (extend-env var (newref val) saved-env) saved-cont))
         (end-cont () (error "Uncaught exception " val))
         (diff1-cont (exp2 saved-env saved-cont) (apply-handler val saved-cont))
         (diff2-cont (val1 saved-cont) (apply-handler val saved-cont))
         (zero1-cont (saved-cont) (apply-handler val saved-cont))
         (let-exp-cont (vars body env saved-cont)
           (apply-handler val saved-cont))
         (list-value-cont (args env saved-cont)
           (apply-handler val saved-cont))
         (list-append-cont (arg saved-cont)
           (apply-handler val saved-cont))
         (if-test-cont (conseq alt env saved-cont)
           (apply-handler val saved-cont))
         (raise1-cont (saved-cont)
           (apply-handler val saved-cont))
         (rator-cont (rands env saved-cont)
           (apply-handler val saved-cont))
         (rands-cont (proc saved-cont)
           (apply-handler val saved-cont))
         (else (error "Unimplemented"))))

(define (the-empty-queue) '())
(define empty? null?)
(define (enqueue queue val)
  (append queue (list val)))
(define (dequeue queue func)
  (cond ((null? queue) (error "attemping to dequeue empty queque"))
        (else (func (car queue) (cdr queue)))))
