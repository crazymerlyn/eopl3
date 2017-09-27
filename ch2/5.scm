(define (empty-env) '())
(define (extend-env var val env)
  (cons (cons var val) env))
(define (apply-env env var)
  (cond ((assoc var env) => cdr)
        (else (error "No binding for" var))))

(define (empty-env? env) (null? env))

(define (has-binding? env var)
  (if (assoc var env) #t #f))


(define (extend-env* vars vals env)
  (if (not (= (length vars) (length vals)))
      (error "vars and vals should be of the same lengh" vars vals)
      (append (map cons vars vals) env)))
