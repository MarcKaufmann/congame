#lang racket/base

(require (for-syntax racket/base)
         component
         congame/components/bot
         koyo/database
         koyo/json
         koyo/profiler
         koyo/session
         koyo/url
         net/url
         racket/contract
         racket/match
         racket/string
         threading
         web-server/http
         congame-web/components/user) ;"user.rkt")

;; auth-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-user

 make-auth-manager
 auth-manager?
 impostor?
 auth-manager-impersonate!
 auth-manager-stop-impersonation!
 auth-manager-login!
 auth-manager-logout!
 wrap-auth-required

 exn:fail:auth-manager?
 exn:fail:auth-manager:unverified?)

(define session-key 'uid)
(define session-impersonator-key 'session-impersonator-uid)

(define/contract current-user
  (parameter/c (or/c false/c user?))
  (make-parameter #f))

(struct exn:fail:auth-manager exn:fail ())
(struct exn:fail:auth-manager:unverified exn:fail:auth-manager ())

(struct auth-manager (sessions users)
  #:methods gen:component [])

(define/contract (make-auth-manager sessions users)
  (-> session-manager? user-manager? auth-manager?)
  (auth-manager sessions users))

(define/contract (impostor?)
  (-> boolean?)
  (not (not (session-ref session-impersonator-key #f))))

(define/contract (auth-manager-impersonate! _am uid)
  (-> auth-manager? id/c void?)
  (session-set! session-key (number->string uid))
  (session-set! session-impersonator-key (number->string (user-id (current-user)))))

(define/contract (auth-manager-stop-impersonation! _am)
  (-> auth-manager? void?)
  (session-set! session-key (session-ref session-impersonator-key))
  (session-remove! session-impersonator-key))

(define/contract (auth-manager-login! am username password)
  (-> auth-manager? non-empty-string? non-empty-string? (or/c false/c user?))
  (match (user-manager-login (auth-manager-users am) username password)
    [#f #f]
    [(and (struct* user ([id id] [verified? verified?])) user)
     (unless verified?
       (raise (exn:fail:auth-manager:unverified "this user is not verified" (current-continuation-marks))))

     (begin0 user
       (session-set! session-key (number->string id)))]))

(define/contract (auth-manager-logout! _am)
  (-> auth-manager? void?)
  (session-remove! session-key))

(define/contract (((wrap-auth-required am req-roles) handler) req)
  (-> auth-manager?
      (-> request? (listof symbol?))
      (-> (-> request? response?)
          (-> request? response?)))

  (with-timing 'auth "wrap-auth-required"
    (let/ec return
      (define roles (req-roles req))
      (define-values (u ok?)
        (cond
          [(null? roles)
           (values #f #t)]

          [(and (equal? roles '(api))
                (and~> (request-headers/raw req)
                       (headers-assq* #"authorization" _)
                       (header-value)
                       (bytes->string/utf-8)
                       (user-manager-lookup/api-key (auth-manager-users am) _)))
           => (lambda (u)
                (values u #t))]

          [(equal? roles '(api))
           (return
            (response/json
             #:code 401
             (hasheq 'error "authorization failed")))]

          [(and~>> (session-ref session-key #f)
                   (string->number)
                   (user-manager-lookup/id (auth-manager-users am)))
           => (lambda (u)
                (values u (case (user-role u)
                            [(admin) #t]
                            [(api)   #f]
                            [(bot)   (equal? roles '(user))]
                            [else    (equal? roles '(user))])))]

          [else
           (values #f #f)]))

      (cond
        [ok?
         (parameterize ([current-user u]
                        [current-user-bot? (and u (eq? 'bot (user-role u)))])
           (handler req))]

        [else
         (redirect-to (make-application-url "login" #:query `((return . ,(url->string (request-uri req))))))]))))
