#lang racket/base

(require congame/components/export
         (only-in congame/components/study
                  current-study-instance-id
                  current-study-stack)
         db
         (prefix-in http: net/http-easy)
         racket/contract
         racket/format
         congame-web/components/auth
         congame-web/components/user)

(provide
 put/identity)

(define-logger identity)

(define/contract (put/identity key value)
  (-> symbol? any/c void?)
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
        'key (->jsexpr key)
        'stack (->jsexpr (current-study-stack))
        'value (->jsexpr value)))
     ; FIXME: do authorization check based on an api user for congame server. use congame_servers table rather than users.
     (define res
       (http:put url #:json data))
     (log-identity-debug "put/identity~n  url: ~a~n  data: ~e" url data)
     (unless (= (http:response-status-code res) 201)
       (error 'put/identity "request failed~n  response: ~a" (http:response-body res)))]

    [else
     ; TODO: Is it a feature or a bug that non-identity users trigger this?
     (log-identity-warning "failed to put/identity~n current user is not an identity user~n  username: ~a~n  key: ~a" (user-username u) key)]))
