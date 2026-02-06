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
 congame-web/pages/auth
 congame-web/pages/common
 congame-web/pages/dashboard
 congame-web/pages/error
 congame-web/pages/push
 congame-web/pages/resource
 congame-web/pages/study)

(reprovide/admin
 congame-web/pages/admin)

(reprovide/api
 congame-web/pages/api)
