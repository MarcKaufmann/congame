#lang racket/base

(require (only-in congame/components/study
                  get-var get-var/instance get-var* get-var*/instance
                  put-var put-var/instance put-var* put-var*/instance)
         conscript/base
         racket/unit
         "congame-sig.rkt"
         "matchmaking-sig.rkt"
         "matchmaking-unit.rkt")

(provide
 (all-defined-out))

(define-values/invoke-unit matchmaking@
  (import congame^)
  (export matchmaking^))
