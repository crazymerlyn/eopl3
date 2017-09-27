(define (empty-env) '())
(define (extend-env var val env)
  (cons (cons var val) env))
(define (apply-env env var)
  (cond ((assoc var env) => cdr)
        (else (error "No binding for" var))))

(define (empty-env? env) (null? env))

(define (has-binding? env var)
  (if (assoc var env) #t #f))
