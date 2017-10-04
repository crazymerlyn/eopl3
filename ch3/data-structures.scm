(define-datatype
  expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?)))

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (error "invalid expval -- expval->bool" val))))

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (error "invalid expval -- expval->num" val))))
