#lang racket/base

(require racket/generic)

(provide
 gen:jsexprable
 jsexprable?
 ->jsexpr)

(define (list->jsexpr xs)
  (map ->jsexpr xs))

(define (hash->jsexpr xs)
  ;; TODO: error if key is not a symbol
  (for/hash ([(k v) (in-hash xs)])
    (values k (->jsexpr v))))

(define-generics jsexprable
  [->jsexpr jsexprable]
  #:fast-defaults
  ([boolean? (define ->jsexpr values)]
   [number?  (define ->jsexpr values)]
   [string?  (define ->jsexpr values)]
   [symbol?  (define ->jsexpr symbol->string)]
   [list?    (define ->jsexpr list->jsexpr)]
   [hash?    (define ->jsexpr hash->jsexpr)]))
