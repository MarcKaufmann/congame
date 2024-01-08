#lang racket/base

(require koyo
         koyo/database/migrator
         koyo/sentry
         net/url
         racket/contract/base
         racket/contract/region
         racket/format
         racket/list
         racket/string
         sentry
         threading
         web-server/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/http
         (only-in web-server/http/response current-header-handler)
         web-server/managers/lru
         web-server/servlet-dispatch
         (prefix-in config: "../config.rkt")
         "../pages/all.rkt"
         "auth.rkt"
         "mail.rkt"
         "prolific.rkt"
         "sentry.rkt"
         "user.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(struct app (dispatcher)
  #:transparent)

(define-logger request)

(define ((make-logging-dispatcher disp) conn req)
  (define start-ms
    (current-inexact-monotonic-milliseconds))
  (parameterize ([current-header-handler
                  (λ (resp)
                    (begin0 resp
                      (log-request-debug
                       "~a /~a ~a [~a] [~a] [~ams]"
                       (request-method req)
                       (string-join (map path/param-path (url-path (request-uri req))) "/")
                       (response-code resp)
                       (request-client-ip req)
                       (and~>
                        (headers-assq* #"user-agent" (request-headers/raw req))
                        (header-value))
                       (~r #:precision '(= 2)
                           (- (current-inexact-monotonic-milliseconds) start-ms)))))])
    (disp conn req)))

(define/contract (make-app auth broker db flashes mailer _migrator sessions users
                           #:debug? [debug? #f]
                           #:memory-threshold [memory-threshold (* 1 1024 1024 1024)]
                           #:static-path [static-path #f])
  (->* (auth-manager? broker? database? flash-manager? mailer? migrator? session-manager? user-manager?)
       (#:debug? boolean?
        #:memory-threshold exact-positive-integer?
        #:static-path (or/c #f path-string?))
       app?)
  (define-values (dispatch reverse-uri req-roles)
    (dispatch-rules+roles
     [("")
      #:name 'dashboard-page
      #:roles (anon)
      (λ (req)
        (if (current-user)
            ((dashboard-page db) req)
            (homepage req)))]

     [("enroll" (integer-arg) (integer-arg))
      #:roles (user)
      (enroll-or-resume-page db)]

     [("login")
      (login-page auth)]

     [("logout")
      (logout-page auth)]

     [("messages")
      #:roles (user)
      (messages-page db)]

     [("messages" (integer-arg))
      #:roles (user)
      (message-page db)]

     [("password-reset")
      (request-password-reset-page flashes mailer users)]

     [("password-reset" (integer-arg) (string-arg))
      (password-reset-page flashes mailer users)]

     [("signup")
      (signup-page auth mailer users)]

     [("admin" "users")
      #:roles (admin)
      (admin:users-study-instance-data-page db)]

     [("tag" (integer-arg))
      #:roles (user)
      (tag-study-instances-page db)]

     [("verify" (integer-arg) (string-arg))
      (verify-page flashes users)]

     [("api" "v1" "study-instances" (integer-arg) "data")
      #:method "put"
      (put-instance-page db)]))

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define wrap-sentry
    (make-sentry-wrapper #:client (current-sentry)))

  (define (stack handler)
    (~> handler
        (wrap-current-sentry-user)
        ((wrap-auth-required auth req-roles))
        ((wrap-browser-locale sessions))
        (wrap-sentry)
        (wrap-prolific)
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)
        ((wrap-errors debug?))))

  (current-broker broker)
  (when debug?
    (current-continuation-key-cookie-secure? #f))
  (current-continuation-wrapper stack)
  (current-reverse-uri-fn reverse-uri)

  (define manager
    (make-threshold-LRU-manager (stack expired-page) memory-threshold))

  (define dispatchers
    (list
     (and static-path (make-static-dispatcher static-path))
     (dispatch/servlet #:manager manager (stack dispatch))
     (dispatch/servlet #:manager manager (stack not-found-page))))

  (app (make-logging-dispatcher
        (apply sequencer:make (filter-map values dispatchers)))))
