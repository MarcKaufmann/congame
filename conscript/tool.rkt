#lang racket/base

(provide
 get-info)

(define (get-info key defval proc)
  (case key
    [(color-lexer)
     (dynamic-require 'conscript/syntax-color 'conscript-lexer)]
    [(drracket:indentation)
     (dynamic-require 'conscript/indent 'indent)]
    [(drracket:keystrokes)
     (dynamic-require 'scribble/private/indentation 'keystrokes)]
    [else
     (proc key defval)]))
