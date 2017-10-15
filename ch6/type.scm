(define (check-equal-type! ty1 ty2 exp)
  (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp)))

(define (report-unequal-types ty1 ty2 exp)
  (error "Types didn't match: ~s != ~a in~%~a"
         (types-to-external-form ty1)
         (types-to-external-form ty2)
         exp))

(define (types-to-external-form ty)
  (cases type ty
         (int-type () 'int)
         (bool-type () 'bool)
         (proc-type (arg-type result-type)
           (list
             (types-to-external-form arg-type)
             '->
             (types-to-external-form result-type)))))
