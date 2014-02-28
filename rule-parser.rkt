#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (VAR))
(define-empty-tokens op-tokens (NOT AND OR IF IFF newline EOF))

;; is this necessesary?
(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))
 (upper-letter (:/ #\A #\Z)))

(define rule-lexer
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space) (rule-lexer input-port)]
   [#\newline (token-newline)]
   ["<=>" (token-IFF)]
   ["=>" (token-IF)]
   ["NOT" (token-NOT)]
   ["AND" (token-AND)]
   ["OR" (token-OR)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]))

(define rule-parser
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   (grammar 
   )))
  
(
"
go <=> (foo AND bar)
bar => #t
go => foo
")