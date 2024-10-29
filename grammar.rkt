#lang racket

(provide (all-defined-out))

(require parser-tools/yacc)

#| Using a macro to define all grammar structs in a concise manner without using eval
   This macro takes a list of struct definitions and generates each one with transparency. |#
(define-syntax define-grammar-structs
  (syntax-rules ()
    [(_ ((name fields) ...))
     (begin
       (define-struct name fields #:transparent) ...)]))

#| Define all grammar structs in one go using the macro defined above. |#
(define-grammar-structs
  ((StartSymbol [lines])                ; Starting point of the grammar, which contains lines
   (Statement [type content])           ; Statement, with a type and content
   (Expr [left op right])               ; Expression with a left operand, operator, and right operand
   (Term [factor])                      ; Term, which consists of a factor
   (Factor [value])                     ; Factor, which contains a value
   (Lines [statements])                 ; Represents multiple lines of statements
   (Statements [statement rest])        ; One or more statements, with a statement and the rest
   (Constant [value])                   ; Constant value (integer, real, or string)
   (IDList [ids])                       ; List of identifiers
   (Value [content])                    ; Value, which could be an expression or constant
   (CompareExpr [left op right])        ; Comparison expression with operands and an operator
   (PrintList [items])                  ; List of items to be printed
   (StringChars [chars])                ; Characters of a string
   (WS [chars])                         ; Whitespace characters
   (NewLine [])                         ; Newline character
   (Whitespace [chars])                 ; Whitespace in general
   (Remark [text])                      ; Comment or remark
   (IntegerList [integers])             ; List of integers
   (Real [value])                       ; Real number
   (String [value])                     ; String value
   (ID [name])                          ; Identifier
   (ValueList [values])                 ; List of values
   (ConstantList [constants])           ; List of constants
   (Integer [value])                    ; Integer value
   (ExpressionList [expressions])       ; List of expressions
   (AndExpr [left right])               ; An AND expression with left and right components
   (NotExpr [expression])               ; A NOT expression
   (AddExpr [left op right])            ; Addition expression with operands and operator
   (MultExpr [left op right])           ; Multiplication expression with operands and operator
   (NegateExpr [value])))               ; Negation expression

#| Define grammar rules using structs to represent
   each non-terminal in the LL(2) grammar.
   
   This section represents how components like
   Statements, Expressions, etc., are structured in Racket. |#

#| <Expression> ::= <AndExpr> OR <Expression>
                   | <AndExpr>
   Creates an expression, handling cases where there might be
   an optional right operand. |#
(define (make-expr left op right)
  (match right
    [#f left]                       ; If there's no right operand, just return the left
    [val (Expr left op val)]))      ; Otherwise, construct an Expr struct with left, operator, and right

#| <CompareExpr> ::= <AddExpr> ('=' | '<>' | '>' | '>=' | '<' | '<=') <CompareExpr>
   | <AddExpr>
   Creates a comparison expression, handling optional right operand. |#
(define (make-compare-expr left op right)
  (match right
    [#f left]
    [val (CompareExpr left op val)]))

#| <AndExpr> ::= <NotExpr> AND <AndExpr>
   | <NotExpr>
   Creates an AND expression, handling optional right operand. |#
(define (make-and-expr left right)
  (match right
    [#f left]
    [val (AndExpr left val)]))

#| <NotExpr> ::= NOT <CompareExpr>
   | <CompareExpr>
   Creates a NOT expression or just returns the comparison expression. |#
(define (make-not-expr expression)
  (if (equal? expression 'NOT)
      (NotExpr expression)
      expression))

#| <AddExpr> ::= <MultExpr> ('+' | '-') <AddExpr>
   | <MultExpr>
   Creates an addition or subtraction expression, handling optional right operand. |#
(define (make-add-expr left op right)
  (match right
    [#f left]
    [val (AddExpr left op val)]))

#| <MultExpr> ::= <NegateExpr> ('*' | '/') <MultExpr>
   | <NegateExpr>
   Creates a multiplication or division expression, handling optional right operand. |#
(define (make-mult-expr left op right)
  (match right
    [#f left]
    [val (MultExpr left op val)]))

#| <NegateExpr> ::= '-' <Value>
   | <Value>
   Creates a negation expression or just returns the value. |#
(define (make-negate-expr value)
  (if (equal? value '-)
      (NegateExpr value)
      value))

#| <Term> ::= <Factor> ('*' | '/') <Term>
   | <Factor>
   Represents a term, which in this context is simply a factor. |#
(define (make-term factor)
  (Term factor))

#| <Factor> ::= '(' <Expression> ')'
   | ID
   | <Constant>
   Creates a factor which can be an expression, an identifier, or a constant. |#
(define (make-factor value)
  (Factor value))

#| <Constant> ::= Integer
   | Real
   | String
   Creates a constant which could be an integer, real number, or string. |#
(define (make-constant value)
  (Constant value))

#| <ID List> ::= ID ',' <ID List>
   | ID
   Creates a list of identifiers. |#
(define (make-id-list ids)
  (IDList ids))

#| <Value> ::= '(' <Expression> ')'
   | ID
   | <Constant>
   Creates a value which could be an expression, an identifier, or a constant. |#
(define (make-value content)
  (Value content))

#| <Print List> ::= <Expression> ';' <Print List>
   | <Expression>
   |
   Creates a list of items to be printed. |#
(define (make-print-list items)
  (PrintList items))

#| <Value List> ::= <Value> ',' <Value List>
   | <Value>
   Creates a list of values. |#
(define (make-value-list values)
  (ValueList values))

#| <Constant List> ::= <Constant> ',' <Constant List>
   | <Constant>
   Creates a list of constants. |#
(define (make-constant-list constants)
  (ConstantList constants))

#| <Integer List> ::= Integer ',' <Integer List>
   | Integer
   Creates a list of integers. |#
(define (make-integer-list integers)
  (IntegerList integers))

#| <Expression List> ::= <Expression> ',' <Expression List>
   | <Expression>
   Creates a list of expressions. |#
(define (make-expression-list expressions)
  (ExpressionList expressions))
