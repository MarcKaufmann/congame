#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide
 transition-graph
 -->)

(define-syntax (--> stx)
  (raise-syntax-error '--> "may only be used within a transition-graph form" stx))

(define-syntax (transition-graph stx)
  (define-splicing-syntax-class arrow
    #:literals (--> unquote)
    (pattern id:id
             #:with e #''id
             #:with (c ...) #'())
    (pattern (unquote e:expr)
             #:with (c ...) #'())
    (pattern (~seq id:id --> a2:arrow)
             #:with e #''id
             #:with (c ...) #'([cons 'id a2.e] a2.c ...)))

  ;; TODO: Error on single entry.
  (syntax-parse stx
    #:literals (-->)
    [(_ [arrows:arrow] ...+)
     #'(list arrows.c ... ...)]))
