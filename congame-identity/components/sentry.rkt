#lang racket/base

(require racket/contract/base
         sentry
         web-server/http
         "auth.rkt"
         "user.rkt")

(provide
 (contract-out
  [wrap-current-sentry-user
   (-> (-> request? response?)
       (-> request? response?))]))

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
