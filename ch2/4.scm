(define (empty-stack) '())
(define (empty-stack? stack) (null? stack))
(define (push val stack) (cons val stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))