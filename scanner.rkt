#lang racket

(require parser-tools/lex "grammar.rkt")

(provide (all-defined-out))

#| Define a scanner for the project using the `parser-tools/lex` package.
   This scanner will be responsible for tokenizing input strings and ensuring
   that they conform to the grammar rules defined in other parts of the project. |#

; Define lexical abbreviations for common patterns
(define-lex-abbrev identifier "[a-zA-Z_][a-zA-Z0-9_]*")
(define-lex-abbrev integer "[0-9]+")
(define-lex-abbrev whitespace "[ \t]+")

#| Define the lexer using the lexical abbreviations above.
   This lexer will tokenize identifiers, numbers, whitespace, and other elements as needed. |#
(define my-lexer
  (lexer
    [identifier (token-name 'ID lexeme)]
    [integer (token-name 'Integer lexeme)]
    [whitespace (token-name 'WS lexeme)]
    [#\newline (token-name 'NewLine)]
    [(eof) (token-name 'EOF)]))

; Function to create tokens from an input stream
(define (create-tokens in)
  (let loop ((token (my-lexer in)) (tokens '()))
    (cond
      [(eof-object? token) (reverse tokens)] ; Return accumulated tokens when EOF is reached.
      [(invalid-character? token) (error "Invalid character detected in input" (token-value token))] ; Handle invalid characters
      [else (loop (my-lexer in) (cons token tokens))])))

; Function to tokenize a string directly
(define (tokenize-string str)
  (create-tokens (open-input-string str)))

; Function to scan lines and tokenize each line
(define (scan-lines lines)
  (map tokenize-string lines))

; Function to accumulate tokens from already tokenized lines
(define (accumulate-tokens tokenized-lines)
  (foldl append '() tokenized-lines))

; Function to scan input lines and accumulate tokens
(define (scan-input input-lines)
  (accumulate-tokens (scan-lines input-lines)))

#| Handling Maybe Types
   As discussed earlier, there are considerations when using "Maybe" types, especially to handle optional or missing values.
   The scanner needs to handle `#f` appropriately to avoid unexpected parsing errors.

   - **Fallback Mechanisms**: Provide default tokens or generate specific error messages when tokens are absent.
   - **Wrapper Functions**: Utilize wrapper functions around the lexer output to systematically handle `#f` cases.
|#

; Example usage of scan-input
; (scan-input "let x = 42\n")

; This scanner will tokenize input strings and validate the components,
; serving as a preprocessing step before passing tokens to the parser.

; Helper function to determine if a token contains invalid characters
(define (invalid-character? token)
  (let ((token-val (token-value token)))
    (regexp-match? #px"[$^@]" token-val)))
