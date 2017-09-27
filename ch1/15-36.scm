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


; list-index: (SchemeVal -> boolean) x List -> Int + #f
; Usage: (list-index pred lst) -> returns the first index for which
;        pred returns a truthy. Returns #f if no such index exists
(define (list-index pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) 0)
        ((list-index pred (cdr lst)) => (lambda (i) (+ i 1)))
        (else #f)))


; every?: (SchemeVal -> boolean) x List -> boolean
; Usage: (every? pred lst) = If pred returns false for any value in lst
;        then return false else true
(define (every? pred lst)
  (if (null? lst)
      #t
      (if (not (pred (car lst)))
          #f
          (every? pred (cdr lst)))))


; exists?: (SchemeVal -> boolean) x List -> boolean
; Usage: (exists? pred lst) = If pred returns truthy for any element of lst
;        then return true else return false
(define (exists? pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (exists? pred (cdr lst)))))


; up: List -> List
; Usage: (up ((x (y)) z a (a b))) = (x (y) z a a b)
(define (up lst)
  (cond ((null? lst) '())
        ((pair? (car lst)) (append (car lst) (up (cdr lst))))
        (else (cons (car lst) (up (cdr lst))))))


; flatten: S-List -> S-List
; Usage: (flatten slist) = slist with all inner parentheses removed
(define (flatten slist)
  (cond ((null? slist) '())
        ((symbol? (car slist)) (cons (car slist) (flatten (cdr slist))))
        (else (append (flatten (car slist)) (flatten (cdr slist))))))


; merge: ListOf(Int) x ListOf(Int) -> ListOf(Int)
; Usage: (merge loi1 loi2) loi1 and loi2 are in increasing order =
;        list containing elements of both loi1 and loi2 in increasing order
(define (merge loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        ((< (car loi1) (car loi2))
         (cons (car loi1) (merge (cdr loi1) loi2)))
        (else
          (cons (car loi2) (merge loi1 (cdr loi2))))))


; sort: ListOf(Int) -> ListOf(Int)
; Usage: (sort loi) = Elements of loi in increasing order
(define (sort loi)
  (cond ((null? loi) '())
        ((null? (cdr loi)) loi)
        (else
          (let* ((n (length loi))
                 (res (split-at loi (quotient n 2))))
            (merge (sort (car res))
                   (sort (cdr res)))))))
(define (split-at lst n)
  (cons (take lst n) (drop lst n)))


(define (sort/predicate pred loi)
  (cond ((null? loi) '())
        ((null? (cdr loi)) loi)
        (else
          (let* ((n (length loi))
                 (res (split-at loi (quotient n 2))))
            (merge/predicate pred
                             (sort/predicate pred (car res))
                             (sort/predicate pred (cdr res)))))))
(define (merge/predicate pred loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        ((pred (car loi1) (car loi2))
         (cons (car loi1) (merge/predicate pred (cdr loi1) loi2)))
        (else
          (cons (car loi2) (merge/predicate pred loi1 (cdr loi2))))))
