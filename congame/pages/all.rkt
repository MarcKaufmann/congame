#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(define-syntax-rule (reprovide/admin mod ...)
  (begin
    (require (prefix-in admin: mod) ...)
    (provide (all-from-out mod) ...)))

(define-syntax-rule (reprovide/api mod ...)
  (begin
    (require (prefix-in api: mod) ...)
    (provide (all-from-out mod) ...)))


(reprovide
 "auth.rkt"
 "common.rkt"
 "dashboard.rkt"
 "resource.rkt"
 "study.rkt")

(reprovide/admin
 "admin.rkt")

(reprovide/api
 "api.rkt")
