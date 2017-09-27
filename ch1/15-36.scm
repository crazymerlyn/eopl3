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


; list-set: List x Int x SchemeVal -> List
; (list-set lst n x) = lst with nth element replaced with x
(define (list-set lst n x)
  (let loop ((cur-lst lst) (cur-n n))
   (if (null? cur-lst)
       (report-list-too-short lst n)
       (if (zero? cur-n)
           (cons x (cdr cur-lst))
           (cons (car cur-lst) (loop (cdr cur-lst) (- cur-n 1)))))))


; count-occurences: Symbol x S-List -> Int
; (count-occurences x slist) = no. of occurences of x in slist
(define (count-occurences x slist)
  (if (null? slist)
      0
      (if (symbol? (car slist))
          (if (eq? x (car slist))
              (+ 1 (count-occurences x (cdr slist)))
              (count-occurences x (cdr slist)))
          (+ (count-occurences x (car slist))
             (count-occurences x (cdr slist))))))


; product: ListOf(Symbol) x ListOf(Symbol) -> ListOf(ListOf(Symbol))
; Usage: (product sos1 sos2) = A list of 2-lists that represents the cartesian
;        product of sos1 and sos2
(define (product sos1 sos2)
  (concatenate
    (map (lambda (s1)
           (map (lambda (s2) (list s1 s2))
                sos2))
         sos1)))


; filter-in: (SchemeVal -> boolean) x List -> List
; Usage: (filter-in pred lst) = lst with only elements for which
;        pred returns a truthy value
(define (filter-in pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter-in pred (cdr lst)))
          (filter-in pred (cdr lst)))))
