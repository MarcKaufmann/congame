#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod) ...)))

(reprovide "example.rkt")
(reprovide "price-lists.rkt")
(reprovide "pjb-pilot.rkt")
