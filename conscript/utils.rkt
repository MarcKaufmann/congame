#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre)
         congame/components/formular

         koyo/haml)

(define-syntax (make-sliders stx)
  (syntax-parse stx
    [(_ (list var ...))
     #'(list
        (haml
         (:div
          (list (string->keyword (symbol->string var ...)) (input-number "Number") ))))]))

(make-sliders '(a b c))
