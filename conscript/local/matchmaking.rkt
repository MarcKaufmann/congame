#lang racket/base

(require conscript/local
         (rename-in
          conscript/local
          [get get-var]
          [put put-var]
          [get* get-var*]
          [get*/instance get-var*/instance]
          [put* put-var*]
          [put*/instance put-var*/instance])
         racket/unit
         "../matchmaking-sig.rkt"
         "../matchmaking-unit.rkt")

(provide
 (all-defined-out))

(define-values/invoke-unit matchmaking@
  (import congame^)
  (export matchmaking^))
