#lang racket/base

(require json
         koyo/http
         koyo/json
         web-server/http
         "../components/shadow.rkt"
         "../components/study-instance.rkt")

(provide
 put-instance-page)

(define ((put-instance-page db) req instance-id)
  (let/ec esc
    (define the-shadow
      (lookup-shadow/key db (bindings-ref (request-bindings/raw req) 'key "")))
    (unless the-shadow
      (esc (response/json
            #:code 401
            (hasheq 'error "invalid API key"))))
    (unless (equal? (shadow-instance-id the-shadow) instance-id)
      (esc (response/json
            #:code 400
            (hasheq 'error "mismatched shadow and instance id"))))
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
       #:user-id (shadow-user-id the-shadow)
       #:server-id (shadow-server-id the-shadow)
       #:instance-id (shadow-instance-id the-shadow)
       #:study-stack (for/list ([id (in-list (ref 'stack))])
                       (string->symbol id))
       #:key (string->symbol (ref 'key))
       #:value (ref 'value)
       #:congame-url (ref 'congame-url))
      (response/json
       #:code 201
       (hasheq 'ok "ok")))))
