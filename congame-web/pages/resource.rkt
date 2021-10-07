#lang racket/base

(require congame/components/resource
         koyo/mime
         racket/port
         web-server/dispatchers/dispatch
         web-server/http)

(provide
 serve-resource-page)

(define (serve-resource-page _req id [subresource #f])
  (define r (get-resource id))
  (unless r
    (next-dispatcher))
  (define full-path
    (if subresource
        (build-path (resource-path r) subresource)
        (resource-path r)))
  (define size-in-bytes
    (file-size full-path))
  (response/output
   #:mime-type (path->mime-type full-path)
   #:headers (list (header #"content-length" (string->bytes/utf-8 (number->string size-in-bytes))))
   (lambda (out)
     (call-with-input-file full-path
       (lambda (in)
         (copy-port in out))))))
