#lang racket/base

(require congame-web/components/auth
         congame-web/components/user
         racket/contract/base
         sentry
         web-server/http)

(provide
 with-sentry
 (contract-out
  [wrap-current-sentry-user
   (-> (-> request? response?)
       (-> request? response?))]))

;; For use outside of any http request handlers when we need to capture
;; exceptions (eg. study tasks). The body is _not_ in tail position with
;; respect to the with-sentry form.
(define-syntax-rule (with-sentry body0 body ...)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (sentry-capture-exception! e)
                     (raise e))])
    body0
    body ...))

(define ((wrap-current-sentry-user hdl) req)
  (cond
    [(current-user)
     => (lambda (u)
          (parameterize ([current-sentry-user
                          (make-sentry-user
                           #:id (number->string (user-id u))
                           #:email (user-username u))])
            (hdl req)))]

    [else
     (hdl req)]))
