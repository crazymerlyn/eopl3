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
  '((program ((arbno module-def) expression) a-program)
    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("(" (separated-list type ",") "->" type ")") proc-type)
    (type ("%tvar-type" number) tvar-type)

    (optional-type ("?") no-type)
    (optional-type (type) a-type)

    (module-def
      ("module" identifier "interface" interface "body" module-body)
      a-module-definition)

    (decl (identifier ":" type) val-decl)
    (interface ("[" (arbno decl) "]") simple-iface)

    (module-body ("[" (arbno definition) "]") defns-module-body)
    (definition (identifier "=" expression) val-defn)

    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp)

    (expression
      ("zero?" "(" expression ")")
      zero?-exp)

    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)

    (expression (identifier) var-exp)
    (expression ("from" identifier "take" identifier) qualified-var-exp)

    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)   

    (expression
      ("letrec" (arbno optional-type identifier "(" (separated-list identifier ",") ":" (separated-list optional-type ",") ")" "=" expression) "in" expression)
      letrec-exp)

    (expression
      ("proc" "(" (separated-list identifier ",") ":" (separated-list optional-type ",") ")" expression)
      proc-exp)

    (expression
      ("(" expression (arbno expression) ")")
      call-exp)

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
