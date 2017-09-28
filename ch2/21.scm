(define-datatype
  environment environment?
  (empty-env)
  (extended-env
    (var symbol?)
    (val (lambda () #t))
    (base-env environment?)))

(define (has-binding? env search-var)
  (cases environment env
         (empty-env () #f)
         (extended-env (var val base-env)
                       (if (eq? var search-var)
                           val
                           (has-binding? base-env search-var)))))
