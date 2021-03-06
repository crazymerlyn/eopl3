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
  '((program (expression) a-program)

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
      ("set-car!" "(" expression "," expression ")")
      set-car-exp)

    (expression
      ("set-cdr!" "(" expression "," expression ")")
      set-cdr-exp)

    (expression
      ("null?" "(" expression ")")
      null?-exp)

    (expression
      ("emptylist")
      emptylist-exp)

    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)

    (expression
      ("try" expression "catch" "(" identifier ")" expression)
      try-exp)

    (expression
      ("spawn" "(" expression ")")
      spawn-exp)

    (expression
      ("raise" expression)
      raise-exp)

    (expression (identifier) var-exp)

    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)   

    (expression
      ("newref" "(" expression ")")
      newref-exp)

    (expression
      ("deref" "(" expression ")")
      deref-exp)

    (expression
      ("set" identifier "=" expression)
      assign-exp)

    (expression
      ("setdynamic" identifier "=" expression "during" expression)
      dynamic-exp)

    (expression
      ("begin" "{" (separated-list expression ";") "}")
      begin-exp)

    (expression
      ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
      letrec-exp)

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
