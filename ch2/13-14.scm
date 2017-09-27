(define (empty-env)
  (list #t
        (lambda (var)
          (error "No binding for" var))))

(define (extend-env var val env)
  (list #f
        (lambda (search-var)
          (if (eq? search-var var)
              val
              ((cadr env) search-var)))))

(define (empty-env? env) (car env))
(define (apply-env env var) ((cadr env) var))
