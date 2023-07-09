#lang racket/base

(require koyo
         koyo/database/migrator
         koyo/sentry
         racket/contract
         racket/list
         threading
         web-server/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
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
  (define (stack handler)
    (~> handler
        (wrap-current-sentry-user)
        ((wrap-auth-required auth req-roles))
        ((wrap-browser-locale sessions))
        ((make-sentry-wrapper config:sentry-dsn
                              #:release config:version
                              #:environment config:environment))
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

  (app (apply sequencer:make (filter-map values dispatchers))))
