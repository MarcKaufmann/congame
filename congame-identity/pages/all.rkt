#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide
 "api.rkt"
 "auth.rkt"
 "common.rkt"
 "dashboard.rkt"
 "message.rkt")
