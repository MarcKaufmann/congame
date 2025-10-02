#lang racket/base

(require (for-syntax racket/base)
         component
         db
         deta/reflect
         koyo/database
         koyo/database/migrator
         koyo/flash
         koyo/hasher
         koyo/job
         koyo/logging
         koyo/mail/postmark
         koyo/sentry
         koyo/server
         koyo/session
         koyo/session/postgres
         racket/runtime-path
         sentry
         sentry/tracing/database
         "components/app.rkt"
         "components/auth.rkt"
         "components/mail.rkt"
         "components/message.rkt"
         "components/user.rkt"
         (prefix-in config: "config.rkt"))

;; System ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path migrations-path
  (build-path 'up "congame-identity-migrations"))

(define-runtime-path static-path
  (build-path 'up "static"))

(define mail-adapter
  (if config:postmark-token
      (make-postmark-mail-adapter (postmark config:postmark-token))
      (make-stub-mail-adapter)))

(define-system prod
  [app (auth broker db flashes mailer migrator sentry sessions users)
       (lambda deps
         (apply make-app deps
                #:debug? config:debug
                #:memory-threshold config:continuation-manager-memory-threshold
                #:static-path static-path))]
  [auth (sessions users) make-auth-manager]
  [broker (db) make-broker]
  [db (make-database-factory
       #:max-connections 100
       (lambda ()
         (trace-connection
          (postgresql-connect
           #:database config:db-name
           #:user     config:db-username
           #:password config:db-password
           #:server   config:db-host
           #:port     config:db-port))))]
  [flashes (sessions) make-flash-manager]
  [hasher (make-argon2id-hasher-factory
           #:parallelism 2
           #:iterations 256
           #:memory 2048)]
  [mail-server (db mailer users) make-mail-server]
  [mailer (make-mailer-factory
           #:adapter mail-adapter
           #:sender config:support-email
           #:common-variables config:common-mail-variables)]
  [migrator (db) (make-migrator-factory migrations-path)]
  [sentry (lambda ()
            (make-sentry-component
             (lambda ()
               (and config:sentry-dsn
                    (make-sentry
                     config:sentry-dsn
                     #:release config:version
                     #:environment config:environment)))))]
  ;; Dummy dependency on mail-server in order to ensure that the
  ;; smtp-server-port file gets created before health checks can pass on
  ;; deployment.
  [server (app mail-server)
          (lambda (app _mail)
            ((make-server-factory
              #:host config:http-host
              #:port config:http-port)
             (app-dispatcher app)))]
  [sessions (db)
            (lambda (db)
              ((make-session-manager-factory
                #:cookie-name config:session-cookie-name
                #:cookie-secure? #f
                #:cookie-same-site 'lax
                #:shelf-life config:session-shelf-life
                #:secret-key config:session-secret-key
                #:store (make-postgres-session-store db))))]
  [users (db hasher) make-user-manager]
  [worker (broker) (make-worker-factory)])


;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 prod-system
 start
 before-reload)

(define (start)
  (define stop-logger
    (start-logger
     #:levels `((app                  . ,config:log-level)
                (mail-adapter         . ,config:log-level)
                (memory-session-store . ,config:log-level)
                (north-adapter        . ,config:log-level)
                (request              . ,config:log-level)
                (server               . ,config:log-level)
                (session              . ,config:log-level)
                (smtp-server          . ,config:log-level)
                (system               . ,config:log-level)
                (worker               . info))))

  (current-system prod-system)
  (with-handlers ([(λ (_) #t)
                   (λ (e)
                     (current-system #f)
                     (stop-logger)
                     (raise e))])
    (system-start prod-system))

  (lambda ()
    (system-stop prod-system)
    (current-system #f)
    (stop-logger)))

(define (before-reload)
  (schema-registry-allow-conflicts? #t))

(module+ main
  (require racket/cmdline
           racket/match
           "health-check.rkt")
  (define mode 'server)
  (command-line
   #:once-each
   [("-c")
    PORT "run a health check on PORT"
    (define port (string->number PORT))
    (unless port
      (error "PORT must be a number"))
    (set! mode `(health-check ,PORT))])
  (match mode
    [`(health-check ,port)
     (health-check port)]
    [_
     (define stop (start))
     (with-handlers ([exn:break? void])
       (sync/enable-break never-evt))
     (stop)]))
