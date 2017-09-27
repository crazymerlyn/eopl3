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


; down: List -> ListOf(List)
; Usage: (down (x1 x2 x3 ...)) = ((x1) (x2) (x3) ...)
(define (down lst)
  (map list lst))


; swapper: Symbol x Symbol x S-List -> S-List
; Usage: (swapper s1 s2 slist) = slist with all instances of s1 replaced
;        with s2 and vice versa
(define (swapper s1 s2 slist)
  (define (swapper-exp s1 s2 sexp)
    (if (symbol? sexp)
        (cond ((eq? sexp s1) s2)
              ((eq? sexp s2) s1)
              (else sexp))
        (swapper s1 s2 sexp)))
  (map (lambda (sexp) (swapper-exp s1 s2 sexp))
       slist))
