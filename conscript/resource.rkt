#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         congame/components/study
         koyo/url
         racket/format)

(provide
 define-static-resource
 resource-uri)

(struct resource (path))

(define-syntax (define-static-resource stx)
  (syntax-parse stx
    [(_ name:id path:string)
     #'(define name (resource path))]))

(define (resource-uri r)
  (make-application-url
   "dsl-resource"
   (~a (current-study-instance-id))
   (resource-path r)))
