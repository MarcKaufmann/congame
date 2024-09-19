#lang racket/base

(require buid
         racket/contract/base
         racket/unit)

(provide
 matchmaking^)

(define-signature matchmaking^
  ((contracted
    [make-matchmaker (-> exact-positive-integer? procedure?)]
    [get-ready-groups (-> (listof buid/c))]
    [get-current-group (-> (or/c #f buid/c))]
    [reset-current-group (-> void?)])))
