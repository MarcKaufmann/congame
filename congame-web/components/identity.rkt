#lang racket/base

(require congame-web/components/auth
         congame-web/components/user
         congame/components/export
         (only-in congame/components/study
                  current-study-instance-id
                  current-study-stack
                  put)
         db
         koyo/url
         (prefix-in http: net/http-easy)
         racket/contract/base
         racket/format)

(provide
 (contract-out
  [put/identity (-> symbol? any/c void?)]))

(define-logger identity)

(define (put/identity key value)
  (define u (current-user))
  (cond
    [(and (not (sql-null? (user-identity-service-url u)))
          (not (sql-null? (user-identity-service-key u))))
     (define url
       (~a (user-identity-service-url u)
           (format "/api/v1/study-instances/~a/data?key=~a"
                   (current-study-instance-id)
                   (user-identity-service-key u))))
     (define data
       (hasheq
        'congame-url (make-application-url)
        'key key
        'stack (current-study-stack)
        'value value))
     (define res (http:put url #:json (->jsexpr data)))
     (log-identity-debug "put/identity~n  url: ~a~n  data: ~e" url data)
     (unless (= (http:response-status-code res) 201)
       (error 'put/identity "request failed~n  response: ~a" (http:response-body res)))]

    [else
     (put #:root '*identity* key value)]))
