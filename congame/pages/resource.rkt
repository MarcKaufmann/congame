#lang racket/base

(require congame/components/resource
         racket/port
         web-server/dispatchers/dispatch
         web-server/http)

(provide
 serve-resource-page)

(define (serve-resource-page _req id)
  (define r (get-resource id))
  (unless r
    (next-dispatcher))
  (response/output
   #:mime-type (resource-mime-type r)
   (lambda (out)
     (call-with-input-file (resource-path r)
       (lambda (in)
         (copy-port in out))))))
