#lang racket/base

(require congame-web/components/auth
         congame-web/components/user
         deta
         koyo/database
         koyo/http
         racket/contract/base
         threading
         web-server/http)

(provide
 (contract-out
  [register-push-subscription-page
   (-> database? (-> request? response?))]))

(define ((register-push-subscription-page db) req)
  (define data (request-json req))
  (define endpoint (hash-ref data 'endpoint))
  (define keys (hash-ref data 'keys))
  (with-database-connection [conn db]
    (~> (current-user)
        (set-user-push-endpoint endpoint)
        (set-user-push-keys keys)
        (update-one! conn _)))
  (response/empty))
