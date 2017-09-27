(define (diff t1 t2)
  (list 'diff t1 t2))
(define (one) (list 'one))
(define (left t) (cadr t))
(define (right t) (caddr t))
(define (one? t) (eq? (car t) 'one))


(define (zero) (diff (one) (one)))
(define (diff-tree->integer n)
  (if (one? n)
      1
      (- (diff-tree->integer (left n))
         (diff-tree->integer (right n)))))

(define (is-zero? n)
  (zero? (diff-tree->integer n)))
(define (successor n)
  (diff n (diff n (one))))

(define (predecessor n)
  (diff n (one)))

(define (diff-tree-plus a b)
  (diff (diff a
              (diff (one) b))
        (diff (diff (one) (one))
              (one))))
