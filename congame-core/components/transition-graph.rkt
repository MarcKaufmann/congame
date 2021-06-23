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

  (syntax-parse stx
    #:literals (-->)
    [(_ [arrows:arrow] ...+)
     (for ([edge-stx (in-list (syntax-e #'((arrows.c ...) ...)))]
           [arrow-stx (in-list (syntax-e #'(arrows ...)))])
       (when (null? (syntax-e edge-stx))
         (raise-syntax-error 'transition-graph "nodes in a graph must point to other nodes" stx arrow-stx)))
     #'(list arrows.c ... ...)]))
