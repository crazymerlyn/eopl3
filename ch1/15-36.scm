; duple: Int x SchemeVal -> List
; Usage: duple n x = (x x ... n times)
(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))


; invert: ListOf(List) -> ListOf(List)
; Usage: invert ((x1 x2) (y1 y2) ...) = ((x2 x1) (y2 y1) ...)
(define (invert lst)
  (if (null? lst)
      '()
      (let ((list2 (car lst)))
       (cons (list (cadr list2) (car list2))
             (invert (cdr lst))))))
