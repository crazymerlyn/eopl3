(define-datatype
  expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (pair-val
    (first expval?)
    (second expval?))
  (proc-val
    (vars list?)
    (body expression?)
    (env environment?))
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



(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val reference?)
    (env environment?))
  (extend-env-rec
    (p-names list?)
    (b-vars list?)
    (bodies list?)
    (env environment?)))

(define (apply-env env var)
  (cases environment env
         (empty-env () (error "No binding for " var))
         (extend-env (saved-var saved-val saved-env)
           (if (eq? var saved-var)
               saved-val
               (apply-env saved-env var)))
         (extend-env-rec (p-names b-vars bodies saved-env)
           (cond ((assoc var (map list p-names b-vars bodies))
                  => (lambda (struct) (proc-val (cadr struct) (caddr struct) env)))
               (else (apply-env saved-env var))))))

(define (extend-env* vars vals env)
  (cond ((null? vars) env)
        ((null? vals) (error "Invalid no. of arguments"))
        (else (extend-env
                (car vars) (car vals)
                (extend-env* (cdr vars) (cdr vals) env)))))

(define (extend-env-with-self-and-super self super env)
  (extend-env
    '%self (newref self)
    (extend-env
      '%super (newref super)
      env)))


(define (empty-store) '())
(define the-store 'uninitialized)
(define (initialize-store!)
  (set! the-store (empty-store)))
(define (reference? v) (integer? v))
(define (newref val)
  (let ((next-ref (length the-store)))
   (set! the-store (append the-store (list val)))
   next-ref))
(define (deref ref)
  (list-ref the-store ref))
(define (setref! ref val)
  (set! the-store
    (letrec
      ((setref-inner
         (lambda (store1 ref1)
           (cond
             ((null? store1) (error "Invalid refernce" ref store1))
             ((zero? ref1) (cons val (cdr store1)))
             (else (cons (car store1)
                         (setref-inner
                           (cdr store1) (- ref1 1))))))))
      (setref-inner the-store ref))))


(define-datatype object object?
  (an-object
    (class-name identifier?)
    (fields (list-of reference?))))

(define (object->class-name obj)
  (cases object obj
         (an-object (class-name fields) class-name)))

(define (object->fields obj)
  (cases object obj
         (an-object (class-name fields) fields)))

(define (new-object class-name)
  (an-object
    class-name
    (map
      (lambda (field-name)
        (newref (list 'uninitialized-field field-name)))
      (class->field-names (lookup-class class-name)))))

(define-datatype method method?
  (a-method
    (vars (list-of identifier?))
    (body expression?)
    (super-name identifier?)
    (field-names (list-of identifier?))))

(define the-class-env '())
(define (add-to-class-env! class-name class)
  (set!
    the-class-env
    (cons (list class-name class) the-class-env)))
(define (lookup-class name)
  (cond ((assq name the-class-env) => cadr)
        (else (error "Unknown class name" name))))

(define (initialize-class-env! c-decls)
  (set! the-class-env
    (list (list 'object (a-class #f '() '()))))
  (for-each initialize-class-decl! c-decls))

(define (initialize-class-decl! c-decl)
  (cases class-decl c-decl
    (a-class-decl (c-name s-name f-names m-decls)
      (let ((f-names (append-field-names
                       (class->field-names (lookup-class s-name))
                       f-names)))
        (add-to-class-env!
          c-name
          (a-class
            s-name f-names
            (merge-method-envs
              (class->method-env (lookup-class s-name))
              (method-decls->method-env
                m-decls s-name f-names))))))))

(define (append-field-names s-f-names f-names)
  (cond ((null? s-f-names) f-names)
        (else
          (cons
            (if (memq (car s-f-names) f-names)
                (fresh-identifier (car s-f-names))
                (car s-f-names))
            (append-field-names
              (cdr s-f-names) f-names)))))

(define fresh-identifier
  (let ((sn 0))
   (lambda (name)
     (set! sn (+ sn 1))
     (string->symbol
       (string-append
         (symbol->string name)
         "%"
         (number->string sn))))))

(define-datatype class class?
  (a-class
    (super-name (maybe identifier?))
    (field-names (list-of identifier?))
    (method-env method-environment?)))

(define (class->field-names cl)
  (cases class cl
         (a-class (a field-names b) field-names)))

(define (class->method-env cl)
  (cases class cl
         (a-class (a b method-env) method-env)))

(define (find-method c-name name)
  (let ((m-env (class->method-env (lookup-class c-name))))
   (let ((maybe-pair (assq name m-env)))
    (if (pair? maybe-pair)
        (cadr maybe-pair)
        (error "method not found" c-name name)))))

(define (method-decls->method-env m-decls super-name field-names)
  (map
    (lambda (m-decl)
      (cases method-decl m-decl
        (a-method-decl (method-name vars body)
          (list method-name
                (a-method vars body super-name field-names)))))
    m-decls))

(define (merge-method-envs super-m-env new-m-env)
  (append new-m-env super-m-env))

(define (method-environment? env)
  (list? env))

(define (maybe pred)
  (lambda (x)
    (or (eq? x #f)
        (pred x))))
