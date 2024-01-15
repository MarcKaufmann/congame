#lang racket/base

(provide
 get-info)

(define (get-info key defval proc)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
    [(drracket:indentation)
     (dynamic-require 'scribble/private/indentation 'determine-spaces)]
    [(drracket:keystrokes)
     (dynamic-require 'scribble/private/indentation 'keystrokes)]
    [else
     (proc key defval)]))
