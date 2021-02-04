#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         file/md5
         racket/runtime-path)

(provide
 define-static-resource
 current-resource-uri-fn
 (struct-out resource)
 resource-uri
 get-resource)

(define all-resources
  (make-hash))

(define current-resource-uri-fn
  (make-parameter
   (lambda (r subr)
     (format "stub://~a/~a" (resource-id r) subr))))

(struct resource (id path)
  #:transparent)

(define-syntax (define-static-resource stx)
  (syntax-parse stx
    [(_ name:id e:expr)
     (with-syntax ([path-name (format-id #'name "~a-path" #'name)])
       (define drp-stmt (datum->syntax stx `(,#'define-runtime-path ,#'path-name ,#'e)))
       #`(begin
           #,drp-stmt
           (define name (resource (hash-file path-name) path-name))
           (register! name)))]))

(define (hash-file p)
  (bytes->string/utf-8
   (if (directory-exists? p)
       (md5 (open-input-string (path->string p)))
       (call-with-input-file p md5))))

(define (register! r)
  (hash-set! all-resources (resource-id r) r))

(define (resource-uri r [subr #f])
  ((current-resource-uri-fn) r subr))

(define (get-resource id)
  (hash-ref all-resources id #f))
