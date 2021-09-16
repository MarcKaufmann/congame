#lang racket/base

(require (for-syntax racket/base)
         congame/components/resource
         congame/components/study
         koyo
         koyo/database/migrator
         koyo/error
         koyo/sentry
         net/url
         racket/contract
         racket/runtime-path
         racket/string
         threading
         web-server/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/dispatchers/filesystem-map
         web-server/http
         web-server/managers/lru
         web-server/servlet-dispatch
         (prefix-in config: "../config.rkt")
         "../pages/all.rkt"
         "auth.rkt"
         "mail.rkt"
         "prolific.rkt"
         "sentry.rkt"
         (prefix-in tpl: "template.rkt")
         "user.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(define-runtime-path static-path
  (build-path 'up 'up "static"))

(define url->path
  (make-url->path static-path))

(define (static-url->path u)
  (url->path (struct-copy url u [path (cdr (url-path u))])))

(define static-dispatcher
  (files:make
   #:url->path static-url->path
   #:path->mime-type path->mime-type))

(struct app (dispatcher))

(define-logger request)

(define ((wrap-logging hdl) req)
  (log-request-debug
   "~a /~a [~a] [~a]"
   (request-method req)
   (string-join (map path/param-path (url-path (request-uri req))) "/")
   (request-client-ip req)
   (and~>
    (headers-assq* #"user-agent" (request-headers/raw req))
    (header-value)))
  (hdl req))

(define/contract (make-app auth broker broker-admin db flashes mailer _migrator sessions users)
  (-> auth-manager? broker? broker-admin? database? flash-manager? mailer? migrator? session-manager? user-manager? app?)
  (define-values (dispatch reverse-uri req-roles)
    (dispatch-rules+roles
     [("")
      #:roles (user)
      #:method (or "get" "post")
      (study-instances-page db)]

     [("admin")
      #:roles (admin)
      (admin:studies-page db)]

     [("admin" "studies" "new")
      #:roles (admin)
      (admin:create-study-page db)]

     [("admin" "studies" (integer-arg))
      #:roles (admin)
      (admin:view-study-page db)]

     [("admin" "studies" (integer-arg) "instances" "new")
      #:roles (admin)
      (admin:create-study-instance-page db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg) "edit")
      #:roles (admin)
      (admin:edit-study-instance-page db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg))
      #:roles (admin)
      (admin:view-study-instance-page db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg) "participants" (integer-arg))
      #:roles (admin)
      (admin:view-study-participant-page auth db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg) "bot-sets" "new")
      #:roles (admin)
      (admin:create-study-instance-bot-sets-page db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg) "bot-sets" (integer-arg))
      #:roles (admin)
      (admin:view-study-instance-bot-set-page db users)]

     [("admin" "jobs" (string-arg) ...)
      #:roles (admin)
      (lambda (req . _args)
        ((broker-admin-handler broker-admin) req))]

     [("admin" "stop-impersonation")
      (admin:stop-impersonation-page auth)]

     [("api" "v1" "studies.json")
      #:roles (api)
      (api:studies db)]

     [("api" "v1" "studies" (integer-arg) "instances.json")
      #:roles (api)
      (api:study-instances db)]

     [("api" "v1" "studies" (integer-arg) "instances" (integer-arg) "participants.json")
      #:roles (api)
      (api:study-participants db)]

     [("study" (string-arg))
      #:roles (user)
      (study-page db)]

     [("login")
      (login-page auth)]

     [("logout")
      (logout-page auth)]

     [("password-reset")
      (request-password-reset-page flashes mailer users)]

     [("password-reset" (integer-arg) (string-arg))
      (password-reset-page flashes mailer users)]

     [("signup")
      (signup-page auth mailer users)]

     [("verify" (integer-arg) (string-arg))
      (verify-page flashes users)]

     [("resource" (string-arg))
      serve-resource-page]

     [("resource" (string-arg) (string-arg))
      serve-resource-page]))

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define (stack handler)
    (~> handler
        ((wrap-auth-required auth req-roles))
        ((wrap-browser-locale sessions))
        ((make-sentry-wrapper config:sentry-dsn
                              #:release config:version
                              #:environment config:environment))
        (wrap-current-sentry-user)
        (wrap-prolific)
        ((wrap-errors config:debug))
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)
        (wrap-logging)))

  (current-broker broker)
  (when config:debug
    (current-continuation-key-cookie-secure? #f))
  (current-continuation-wrapper stack)
  (current-reverse-uri-fn reverse-uri)
  (current-resource-uri-fn
   (lambda (r subr)
     (if subr
         (reverse-uri 'serve-resource-page (resource-id r) subr)
         (reverse-uri 'serve-resource-page (resource-id r)))))
  (current-production-error-page production-error-page)
  (current-git-sha config:git-sha)
  (current-xexpr-wrapper tpl:page/xexpr)

  (define manager
    (make-threshold-LRU-manager (stack expired-page) (* 1024 1024 512)))

  (app (sequencer:make
        (filter:make #rx"^/static/.+$" static-dispatcher)
        (dispatch/servlet #:manager manager (stack dispatch))
        (dispatch/servlet #:manager manager (stack not-found-page)))))
