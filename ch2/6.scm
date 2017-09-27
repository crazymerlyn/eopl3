(define (empty-env) (lambda (var) (error "No binding for" var)))
(define (extend-env var val env)
  (lambda (search-var)
    (if (eq? search-var var)
        val
        (env search-var))))
(define (apply-env env var) (env var))
