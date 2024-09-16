#lang racket/base

(require conscript/local
         (rename-in
          conscript/local
          [get get-var]
          [put put-var]
          [get/instance get-var/instance]
          [put/instance put-var/instance]
          [get* get-var*]
          [get*/instance get-var*/instance]
          [put* put-var*]
          [put*/instance put-var*/instance])
         racket/unit
         "../congame-sig.rkt"
         "../matchmaking-sig.rkt"
         "../matchmaking-unit.rkt")

(provide
 (all-defined-out))

(define-values/invoke-unit matchmaking@
  (import congame^)
  (export matchmaking^))
