#lang racket/base

(require file/sha1
         net/http-easy
         racket/file
         racket/format
         racket/string
         racket/tcp
         "process.rkt"
         "util.rkt")

(provide
 (struct-out server-context)
 call-with-benchmark-server
 configure-cli-for-url!
 create-study-instance!
 upload-study!)

(struct server-context
  (url database-name home environment log-path api-key)
  #:transparent)

(define database-prefix "congame_llm_bench_")

(define (safe-fragment value [max-length 24])
  (define normalized
    (regexp-replace* #px"[^a-z0-9]+" (string-downcase (~a value)) "_"))
  (define trimmed (string-trim normalized "_"))
  (substring trimmed 0 (min max-length (string-length trimmed))))

(define (make-database-name run-id phase)
  (define digest (sha1 (open-input-string run-id)))
  (string-append database-prefix
                 (safe-fragment phase 8)
                 "_"
                 (substring digest 0 16)))

(define (assert-safe-database-name! name)
  (unless (and (string-prefix? name database-prefix)
               (regexp-match? #px"^[a-z0-9_]+$" name))
    (error 'database "refusing unsafe benchmark database name: ~s" name)))

(define (port-available-listener port)
  (with-handlers ([exn:fail:network? (lambda (_) #f)])
    (tcp-listen port 4 #t "127.0.0.1")))

(define (allocate-port-pair)
  (let loop ([attempts 500])
    (when (zero? attempts)
      (error 'allocate-port-pair "could not find two consecutive open ports"))
    (define port (+ 20000 (random 35000)))
    (define first (port-available-listener port))
    (cond
      [first
       (define second (port-available-listener (add1 port)))
       (cond
         [second
          (tcp-close second)
          (tcp-close first)
          port]
         [else
          (tcp-close first)
          (loop (sub1 attempts))])]
      [else (loop (sub1 attempts))])))

(define (postgres-environment [admin? #f])
  (define password
    (if admin?
        (or (getenv "PG_ADMIN_PASSWORD") (getenv "PGPASSWORD") "congame")
        (or (getenv "PGPASSWORD") "congame")))
  (make-environment
   (hasheq
    'PGHOST (or (getenv "PGHOST") "127.0.0.1")
    'PGPORT (or (getenv "PGPORT") "5432")
    'PGUSER (if admin?
                (or (getenv "PG_ADMIN_USER") (getenv "USER") "postgres")
                (or (getenv "PGUSER") "congame"))
    'PGPASSWORD password)))

(define (postgres-settings)
  (values (or (getenv "PGHOST") "127.0.0.1")
          (or (getenv "PGPORT") "5432")
          (or (getenv "PGUSER") "congame")
          (or (getenv "PGPASSWORD") "congame")
          (or (getenv "PG_ADMIN_USER") (getenv "USER") "postgres")))

(define (drop-database! name)
  (assert-safe-database-name! name)
  (define-values (host port _user _password admin) (postgres-settings))
  (run-command/capture
   "dropdb"
   (list "--if-exists" "-h" host "-p" port "-U" admin name)
   #:environment (postgres-environment #t)))

(define (create-database! name)
  (assert-safe-database-name! name)
  (define-values (host port user _password admin) (postgres-settings))
  (run-command/capture
   "createdb"
   (list "-h" host "-p" port "-U" admin "-O" user name)
   #:environment (postgres-environment #t)))

(define (psql/capture context sql #:check? [check? #t])
  (define-values (host port _user _password admin) (postgres-settings))
  (run-command/capture
   "psql"
   (list "-h" host
         "-p" port
         "-U" admin
         "-d" (server-context-database-name context)
         "-X" "-tA" "-v" "ON_ERROR_STOP=1"
         "-c" sql)
   #:environment (postgres-environment #t)
   #:check? check?))

(define (server-environment database-name port run-home run-root)
  (define-values (host pg-port user password _admin) (postgres-settings))
  (define url (format "postgres://~a:~a@~a:~a/~a"
                      user password host pg-port database-name))
  (make-environment
   (hasheq
    'DATABASE_URL url
    'CONGAME_DEBUG "x"
    'CONGAME_WEB_HTTP_HOST "0.0.0.0"
    'CONGAME_WEB_HTTP_PORT (~a port)
    'CONGAME_WEB_URL_SCHEME "http"
    'CONGAME_WEB_URL_HOST "127.0.0.1"
    'CONGAME_WEB_URL_PORT (~a port)
    'CONGAME_WEB_LOG_LEVEL "warning"
    'CONGAME_WEB_ENVIRONMENT "llm-benchmark"
    'CONGAME_WEB_UPLOADS_DIR (path-string (build-path run-root "uploads"))
    'CONGAME_WEB_SESSION_PATH (path-string (build-path run-root "session.rktd"))
    'CONGAME_WEB_SESSION_SECRET_KEY_PATH (path-string (build-path run-root "session-secret"))
    'PLTUSERHOME (path-string run-home)
    'HOME (path-string run-home)
    'CONGAME_CLI_NO_OPEN "1")))

(define (wait-for-server! context)
  (let loop ([attempts 60] [last-error #f])
    (define status
      (with-handlers ([exn:fail? (lambda (e) (set! last-error e) #f)])
        (response-status-code (get (server-context-url context)))))
    (cond
      [(equal? status 200) (void)]
      [(zero? attempts)
       (define server-log
         (if (file-exists? (server-context-log-path context))
             (file->string (server-context-log-path context))
             "<server log unavailable>"))
       (error 'wait-for-server!
              "Congame did not become ready: ~a\nserver log:\n~a"
              (if last-error (exn-message last-error) (format "HTTP ~a" status))
              server-log)]
      [else
       (sleep 0.5)
       (loop (sub1 attempts) last-error)])))

(define (read-api-key context)
  (let loop ([attempts 20])
    (define-values (result stdout _stderr)
      (psql/capture
       context
       (string-append
        "UPDATE users "
        "SET roles = array_append(roles, 'api') "
        "WHERE username = 'admin@congame.local' AND NOT roles @> ARRAY['api']; "
        "SELECT api_key FROM users WHERE username = 'admin@congame.local';")
       #:check? #f))
    (define lines
      (filter (lambda (line) (not (string=? line "")))
              (string-split (string-trim stdout) "\n")))
    (define keys
      (filter (lambda (line) (regexp-match? #px"^[0-9a-f]{56}$" line)) lines))
    (cond
      [(and (zero? (command-result-exit-code result)) (pair? keys))
       (car keys)]
      [(zero? attempts)
       (error 'read-api-key "local admin account was not created")]
      [else
       (sleep 0.25)
       (loop (sub1 attempts))])))

(define (configure-cli-login! context server-url api-key)
  (define expression
    (format
     "(begin (require racket/file) (put-preferences '(congame-cli:key) (list (cons ~s ~s))))"
     server-url
     api-key))
  (run-command/capture
   "racket"
   (list "-e" expression)
   #:environment (server-context-environment context)))

(define (configure-cli-for-url! context server-url)
  (configure-cli-login! context server-url (server-context-api-key context)))

(define (call-with-benchmark-server repository-root run-root run-id phase proc)
  (define phase-root (build-path run-root (safe-fragment phase)))
  (define server-home (build-path phase-root "server-home"))
  (define cli-home (build-path phase-root "cli-home"))
  (ensure-directory! server-home)
  (ensure-directory! cli-home)
  (ensure-directory! (build-path phase-root "uploads"))
  (define database-name (make-database-name run-id phase))
  (define port (allocate-port-pair))
  (define log-path (build-path phase-root "server.log"))
  (define server-proc #f)
  (define log-port #f)
  (define context #f)
  (dynamic-wind
    void
    (lambda ()
      (drop-database! database-name)
      (create-database! database-name)
      (define server-env
        (server-environment database-name port server-home phase-root))
      (define client-env
        (make-environment
         (hasheq 'PLTUSERHOME (path-string cli-home)
                 'HOME (path-string cli-home))
         #:base server-env))
      (set! log-port (open-output-file log-path #:exists 'truncate/replace))
      (set! server-proc
            (start-command
             "racket"
             (list "congame-web/dynamic.rkt" "--mode" "local")
             #:cwd repository-root
             #:environment server-env
             #:stdout log-port
             #:stderr 'stdout))
      (set! context
            (server-context
             (format "http://127.0.0.1:~a" port)
             database-name
             cli-home
             client-env
             log-path
             #f))
      (wait-for-server! context)
      (set! context
            (struct-copy server-context context
                         [api-key (read-api-key context)]))
      (configure-cli-for-url! context (server-context-url context))
      (proc context))
    (lambda ()
      (when server-proc
        (stop-command-group! server-proc #:grace-seconds 10))
      (when log-port (close-output-port log-port))
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (eprintf "warning: failed to drop benchmark database ~a: ~a\n"
                                  database-name (exn-message e)))])
        (drop-database! database-name)))))

(define (upload-study! context study-id study-path
                       #:cwd [cwd (current-directory)]
                       #:stdout [stdout (current-output-port)]
                       #:stderr [stderr (current-error-port)]
                       #:timeout-seconds [timeout-seconds 120])
  (run-command
   "raco"
   (list "congame" "upload" study-id (path-string study-path))
   #:cwd cwd
   #:environment (server-context-environment context)
   #:stdout stdout
   #:stderr stderr
   #:timeout-seconds timeout-seconds))

(define (create-study-instance! context study-id instance-slug)
  (unless (regexp-match? #px"^[a-zA-Z0-9_-]+$" instance-slug)
    (error 'create-study-instance! "unsafe instance slug: ~s" instance-slug))
  (unless (regexp-match? #px"^[a-zA-Z0-9_-]+$" study-id)
    (error 'create-study-instance! "unsafe study id: ~s" study-id))
  (define sql
    (format
     (string-append
      "INSERT INTO study_instances (study_id, owner_id, name, slug, status) "
      "SELECT id, owner_id, '~a', '~a', 'active' FROM studies "
      "WHERE slug = 'cli-~a' RETURNING id;")
     instance-slug instance-slug study-id))
  (define-values (_result stdout _stderr) (psql/capture context sql))
  (define output-lines
    (filter (lambda (line) (not (string=? line "")))
            (string-split (string-trim stdout) "\n")))
  (define id (if (pair? output-lines) (car output-lines) ""))
  (when (string=? id "")
    (error 'create-study-instance! "uploaded study was not found: ~a" study-id))
  id)
