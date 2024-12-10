#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre))

(provide
 define-var-box)

(define-syntax (define-var-box stx)
  (syntax-parse stx
    [(_ id:id var-id:id)
     #'(define id
         (case-lambda
           [() var-id]
           [(v) (set! var-id v)]))]))
