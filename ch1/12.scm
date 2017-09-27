(define (subst s1 s2 slist)
  (if (null? slist)
      '()
      (cons
        (if (symbol? (car slist))
            (if (eq? (car slist) s1) s2 (car slist))
            (subst s1 s2 (car slist)))
        (subst s1 s2 (cdr slist)))))
