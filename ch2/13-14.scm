(define (empty-env)
  (list #t
        (lambda (var)
          (error "No binding for" var))
        (lambda (var) #f)))

(define (extend-env var val env)
  (list #f
        (lambda (search-var)
          (if (eq? search-var var)
              val
              ((cadr env) search-var)))
        (lambda (search-var)
          (if (eq? search-var var)
              #t
              ((caddr env) search-var)))))

(define (empty-env? env) (car env))
(define (apply-env env var) ((cadr env) var))
(define (has-binding? env var) ((caddr env) var))
