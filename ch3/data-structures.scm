(define-datatype
  expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (pair-val
    (first expval?)
    (second expval?))
  (null-val))

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (error "invalid expval -- expval->bool" val))))

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (error "invalid expval -- expval->num" val))))

(define (expval->pair val)
  (cases expval val
         (pair-val (first second) (cons first second))
         (else (error "invalid listval -- expval->list" val))))

(define (list->pairval vals)
  (if (null? vals)
      (null-val)
      (pair-val (car vals) (list->pairval (cdr vals)))))


(define (expval-is-null? val)
  (cases expval val
         (null-val () #t)
         (else #f)))
