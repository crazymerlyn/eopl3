; duple: Int x SchemeVal -> List
; Usage: duple n x = (x x ... n times)
(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))
