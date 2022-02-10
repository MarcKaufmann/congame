#lang racket/base

(require racket/contract
         sentry
         web-server/http
         "auth.rkt"
         "user.rkt")

(provide
 wrap-current-sentry-user)

(define/contract ((wrap-current-sentry-user hdl) req)
  (-> (-> request? response?)
      (-> request? response?))
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
