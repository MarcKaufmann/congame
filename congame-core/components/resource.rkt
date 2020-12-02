#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         file/md5
         koyo/mime)

(provide
 define-static-resource
 current-resource-uri-fn
 (struct-out resource)
 resource-uri
 get-resource)

(define all-resources
  (make-hash))

(define current-resource-uri-fn
  (make-parameter #f))

(struct resource (id mime-type path)
  #:transparent)

(define-syntax (define-static-resource stx)
  (syntax-parse stx
    [(_ name:id e:expr)
     #'(begin
         (define p e)
         (define name (resource (hash-file p) (path->mime-type p) p))
         (register! name))]))

(define (hash-file p)
  (bytes->string/utf-8 (call-with-input-file p md5)))

(define (register! r)
  (hash-set! all-resources (resource-id r) r))

(define (resource-uri r)
  ((current-resource-uri-fn) r))

(define (get-resource id)
  (hash-ref all-resources id #f))
