#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         congame/components/study
         koyo/url
         racket/format)

(provide
 resource?
 define-static-resource
 resource-uri)

(define registry (make-hash))
(define current-track-resources?
  (make-parameter #f))
(module+ private
  (provide registry current-track-resources?))

(struct resource (path))

(define-syntax (define-static-resource stx)
  (syntax-parse stx
    [(_ name:id path:string)
     #'(begin
         (define name (resource path))
         (when (current-track-resources?)
           (hash-set! registry path #t)))]))

(define (resource-uri r [subr #f])
  (apply
   make-application-url
   (append
    `("dsl-resource"
      ,(~a (current-study-instance-id))
      ,(resource-path r))
    (if subr (list subr) '()))))
