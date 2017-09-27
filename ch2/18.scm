(define (number->sequence n) (list n '() '()))
(define (current-element lst) (car lst))
(define (move-to-the-left lst)
  (if (at-left-end? lst)
      (error "Can't move any more to the left" lst)
      (list (car (cadr lst))
            (cdr (cadr lst))
            (cons (car lst) (caddr lst)))))
(define (move-to-the-right lst)
  (if (at-right-end? lst)
      (error "Can't move any more to the right" lst)
      (list (car (caddr lst))
            (cons (car lst) (cadr lst))
            (cdr (caddr lst)))))

(define (insert-to-left lst n)
  (list (car lst) (cons n (cadr lst)) (caddr lst)))

(define (insert-to-right lst n)
  (list (car lst) (cadr lst) (cons n (caddr lst))))

(define (at-left-end? lst) (null? (cadr lst)))
(define (at-right-end? lst) (null? (caddr lst)))

