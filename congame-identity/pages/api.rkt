#lang racket/base

(require json
         koyo/http
         koyo/json
         web-server/http
         "../components/study-instance.rkt"
         "../components/user.rkt")

(provide
 put-instance-page)

;; TODO: Look up congame-server and instance ids off of the shadow and
;; store them with the instance data.
(define ((put-instance-page db users) req instance-id)
  (cond
    [(user-manager-lookup/key users (bindings-ref (request-bindings/raw req) 'key ""))
     => (lambda (u)
          (define data
            (string->jsexpr
             (bytes->string/utf-8
              (request-post-data/raw req))))
          (define (ref id)
            (hash-ref data id (λ () (error 'ref "field '~a' is required" id))))

          (with-handlers ([(lambda (e)
                             (and (exn:fail? e)
                                  (regexp-match? #rx"field.*required" (exn-message e))))
                           (lambda (e)
                             (response/json
                              #:code 400
                              (hasheq 'error (exn-message e))))])
            (put-study-instance-data
             db
             #:user-id (user-id u)
             #:instance-id instance-id
             #:study-stack (for/list ([id (in-list (ref 'stack))])
                             (string->symbol id))
             #:key (string->symbol (ref 'key))
             #:value (ref 'value)
             #:congame-url (ref 'congame-url))
            (response/json
             #:code 201
             (hasheq 'ok "ok"))))]
    [else
     (response/json
      #:code 401
      (hasheq 'error "invalid API key"))]))
