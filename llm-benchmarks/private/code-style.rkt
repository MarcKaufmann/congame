#lang racket/base

(require syntax-color/racket-lexer
         syntax-color/scribble-lexer)

(provide
 (struct-out code-span)
 code-spans)

(struct code-span (start end color) #:transparent)

(define (token-color type lexeme)
  (case type
    [(comment sexp-comment) "#687782"]
    [(string) "#218044"]
    [(constant) "#A65317"]
    [(hash-colon-keyword) "#9A3878"]
    [(parenthesis) "#6B7280"]
    [(error) "#C03639"]
    [(symbol)
     (if (member lexeme
                 '("define" "define-values" "lambda" "λ" "let" "let*" "letrec"
                   "if" "cond" "case" "when" "unless" "require" "provide"
                   "struct" "match" "for" "for/list" "set!" "defvar" "defstep"
                   "defstudy" "form+submit"))
         "#773BB1"
         "#205D98")]
    [else #f]))

(define (code-spans source)
  (define lex
    (if (regexp-match? #px"^#lang[ \t]+(?:conscript|at-exp)(?:[ \t\r\n]|$)" source)
        scribble-lexer
        racket-lexer*))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([mode #f]
             [spans null])
    (define-values (lexeme attributes _paren start end _backup next-mode) (lex in 0 mode))
    (cond
      [(eof-object? lexeme) (reverse spans)]
      [else
       (define type
         (if (hash? attributes)
             (hash-ref attributes 'type 'other)
             attributes))
       (define color
         (if (and (hash? attributes) (hash-ref attributes 'comment? #f))
             (token-color 'comment lexeme)
             (token-color type lexeme)))
       (loop next-mode
             (if color
                 (cons (code-span (sub1 start) (sub1 end) color) spans)
                 spans))])))
