(load "../r5rs.scm")
(load "../sllgen.scm")
(load "../define-datatype.scm")

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program ((arbno class-decl) expression) a-program)

    (class-decl
      ("class" identifier "extends" identifier
       (arbno "field" identifier) (arbno method-decl))
      a-class-decl)

    (method-decl
      ("method" identifier "(" (separated-list identifier ",") ")" expression)
      a-method-decl)

    (expression
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier "(" (separated-list expression ",") ")")
      method-call-exp)

    (expression
      ("super" identifier "(" (separated-list expression ",") ")")
      super-call-exp)

    (expression
      ("instanceof" expression identifier)
      instanceof-exp)

    (expression
      ("cast" expression identifier)
      cast-exp)

    (expression ("self") self-exp)

    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp)

    (expression
      ("zero?" "(" expression ")")
      zero?-exp)


    (expression
      ("list" "(" (separated-list expression ",") ")")
      list-exp)

    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp)

    (expression
      ("car" "(" expression ")")
      car-exp)

    (expression
      ("cdr" "(" expression ")")
      cdr-exp)

    (expression
      ("null?" "(" expression ")")
      null?-exp)

    (expression
      ("emptylist")
      emptylist-exp)

    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)

    (expression (identifier) var-exp)

    (expression
      ("set" identifier "=" expression)
      assign-exp)

    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)   

    (expression
      ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
      letrec-exp)

    (expression
      ("begin" "{" (separated-list expression ";") "}")
      begin-exp)

    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)

    (expression
      ("(" expression (arbno expression) ")")
      call-exp)

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
