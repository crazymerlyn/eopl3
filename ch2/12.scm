(define (empty-stack)
  (list #t
        (lambda () (error "Can't find top element of empty stack"))
        (lambda () (error "Can't pop element from emty stack"))))
(define (empty-stack? stack) (car stack))
(define (push val stack)
  (list #f
        (lambda () val)
        (lambda () stack)))
(define (pop stack) (caddr stack))
(define (top stack) (cadr stack))
