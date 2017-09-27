(define (subst s1 s2 slist)
  (map (lambda (s-exp) (subst-in-s-exp s1 s2 s-exp))
       slist))

(define (subst-in-s-exp s1 s2 s-exp)
  (if (symbol? s-exp)
      (if (eq? s1 s-exp) s2 s-exp)
      (subst s1 s2 s-exp)))
