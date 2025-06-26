#lang racket/base

(require (for-syntax racket/base)
         component
         congame-web/components/app
         congame-web/components/auth
         congame-web/components/mail
         congame-web/components/replication
         congame-web/components/study-bot
         congame-web/components/upload
         congame-web/components/user
         (prefix-in config: congame-web/config)
         congame/components/registry
         congame/components/study
         db
         (prefix-in dbg: debugging/server)
         deta/reflect
         koyo/continuation
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
         racket/contract/base
         racket/contract/region
         racket/file
         racket/runtime-path
         sentry
         sentry/tracing/database
         setup/getinfo
         web-server/safety-limits)

;; System ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path migrations-path
  (build-path 'up "congame-web-migrations"))

(define mail-adapter
  (if config:postmark-token
      (make-postmark-mail-adapter (postmark config:postmark-token))
      (make-stub-mail-adapter)))

(define-system prod
  [app (auth bot-manager broker broker-admin db flashes mailer migrator params replications sentry sessions uploader users) make-app]
  [auth (sessions users) make-auth-manager]
  [bot-manager (db users) make-bot-manager]
  ;; TODO: Check this still holds.
  ;; Some of our jobs depend on the mailer so we need the explicit
  ;; dep. here to avoid running into issues like:
  ;; https://github.com/MarcKaufmann/projection-bias-experiment/issues/57
  [broker (db mailer params)
          (lambda (db _mailer _params)
            (make-broker db))]
  [broker-admin (broker) (make-broker-admin-factory "/admin/jobs")]
  [db (make-database-factory
       (lambda ()
         (trace-connection
          (postgresql-connect
           #:database config:db-name
           #:user     config:db-username
           #:password config:db-password
           #:server   config:db-host
           #:port     config:db-port))))]
  [flashes (sessions) make-flash-manager]
  [hasher (make-argon2id-hasher-factory)]
  [mailer (make-mailer-factory #:adapter mail-adapter
                               #:sender config:support-email
                               #:common-variables config:common-mail-variables)]
  [migrator (db) (make-migrator-factory migrations-path)]
  [params () (lambda ()
               (current-continuation-key-cookie-secure? (not config:debug))
               (current-git-sha config:git-sha)
               (void))]
  [replications (db hasher) (λ (db hasher)
                              (make-replication-manager db hasher migrations-path))]
  [sentry (lambda ()
            (make-sentry-component
             (lambda ()
               (and config:sentry-dsn
                    (make-sentry
                     config:sentry-dsn
                     #:release config:version
                     #:environment config:environment)))))]
  [server (app) (compose1
                 (make-server-factory #:host config:http-host
                                      #:port config:http-port
                                      #:limits (make-safety-limits
                                                #:max-form-data-file-length config:http-max-file-size
                                                #:form-data-file-memory-threshold (* 25 1024 1024)))
                 app-dispatcher)]
  [sessions (make-session-manager-factory #:cookie-name config:session-cookie-name
                                          #:cookie-secure? #f
                                          #:cookie-same-site 'lax
                                          #:shelf-life config:session-shelf-life
                                          #:secret-key config:session-secret-key
                                          #:store (make-memory-session-store
                                                   #:ttl (* 1 86400)
                                                   #:file-path config:session-path))]
  [uploader () (λ () (make-uploader config:uploads-dir))]
  [users (db hasher) make-user-manager]
  [worker (broker) (make-worker-factory)])


;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 prod-system
 start
 before-reload)

(define/contract (start)
  (-> (-> void?))

  (define dbg:stop
    (dbg:serve
     #:host (if (equal? config:environment "dev")
                "127.0.0.1"
                "0.0.0.0")))
  (define stop-logger
    (start-logger
     #:levels `((app                  . ,config:log-level)
                (conscript            . ,config:log-level)
                (dsl                  . ,config:log-level)
                (identity             . ,config:log-level)
                (jsexprable           . ,config:log-level)
                (marionette           . ,config:log-level)
                (mail-adapter         . ,config:log-level)
                (memory-session-store . ,config:log-level)
                (north-adapter        . ,config:log-level)
                (request              . ,config:log-level)
                (server               . ,config:log-level)
                (session              . ,config:log-level)
                (study                . ,config:log-level)
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
    (stop-logger)
    (dbg:stop)))

(define (before-reload)
  ;; To avoid needing to manually bust caches when adding new studies,
  ;; we check if the set of registered studies has changed and then
  ;; delete the compiled code for `congame-web/studies/all` and
  ;; trigger a full restart of the development server `(exit 0)'.
  (unless (equal? (get-registered-studies) *known-registered-studies*)
    (delete-directory/files "congame-web/studies/compiled")
    (exit 0))
  (study-registry-allow-conflicts? #t)
  (schema-registry-allow-conflicts? #t))

(define (get-registered-studies)
  (for*/list ([path (find-relevant-directories '(congame-studies))]
              [desc (in-list ((get-info/full path) 'congame-studies))])
    desc))

(define *known-registered-studies*
  (get-registered-studies))

(module+ main
  (require racket/cmdline
           racket/lazy-require)
  (lazy-require
   ("local.rkt" [(setup local:setup)]))
  (define mode 'standard)
  (command-line
   #:once-each
   [("--mode" "-m")
    MODE "the mode the server is run in"
    (set! mode (string->symbol MODE))])
  (define stop (start))
  (case mode
    [(local) (local:setup)]
    [else (void)])
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt))
  (stop))
