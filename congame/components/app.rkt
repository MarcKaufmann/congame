#lang racket/base

(require (for-syntax racket/base)
         component
         congame/components/resource
         congame/components/study
         koyo
         koyo/database/migrator
         koyo/sentry
         net/url
         racket/contract
         racket/runtime-path
         threading
         web-server/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/dispatchers/filesystem-map
         web-server/managers/lru
         web-server/servlet-dispatch
         (prefix-in config: congame-web/config ) ;"../config.rkt")
         "../pages/all.rkt"
         congame-web/components/auth ;"auth.rkt"
         congame-web/components/mail ;"mail.rkt"
         congame-web/components/sentry ;"sentry.rkt"
         congame-web/components/template ;"template.rkt"
         congame-web/components/user) ;"user.rkt")

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

(struct app (dispatcher)
  #:methods gen:component [])

(define/contract (make-app auth broker broker-admin db flashes mailer _migrator sessions users)
  (-> auth-manager? broker? broker-admin? database? flash-manager? mailer? migrator? session-manager? user-manager? app?)
  (define-values (dispatch reverse-uri req-roles)
    (dispatch-rules+roles
     [("")
      #:roles (user)
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
      (admin:view-study-participant-page db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg) "bot-sets" "new")
      #:roles (admin)
      (admin:create-study-instance-bot-sets-page db)]

     [("admin" "studies" (integer-arg) "instances" (integer-arg) "bot-sets" (integer-arg))
      #:roles (admin)
      (admin:view-study-instance-bot-set-page db)]

     [("admin" "jobs" (string-arg) ...)
      #:roles (admin)
      (lambda (req . _args)
        ((broker-admin-handler broker-admin) req))]

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
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)))

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
  (current-git-sha config:git-sha)
  (current-xexpr-wrapper page/xexpr)

  (define manager
    (make-threshold-LRU-manager (stack expired-page) (* 1024 1024 512)))

  (app (sequencer:make
        (filter:make #rx"^/static/.+$" static-dispatcher)
        (dispatch/servlet #:manager manager (stack dispatch))
        (dispatch/servlet #:manager manager (stack not-found-page)))))
