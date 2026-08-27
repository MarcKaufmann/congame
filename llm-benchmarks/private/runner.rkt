#lang racket/base

(require racket/file
         racket/format
         racket/list
         racket/port
         racket/string
         "config.rkt"
         "git.rkt"
         "grading.rkt"
         "pi-stream-view.rkt"
         "process.rkt"
         "results.rkt"
         "server.rkt"
         "util.rkt"
         "workspace.rkt")

(provide run-one! run-suite!)

(define (short-slug run-id)
  (define cleaned
    (regexp-replace* #px"[^a-zA-Z0-9]+" (string-downcase run-id) "-"))
  (define nonce (last (string-split run-id "__")))
  (format "bench-~a-~a"
          (substring cleaned 0 (min 23 (string-length cleaned)))
          nonce))

(define (copy-server-log! context result-path phase)
  (define destination-directory (build-path result-path "server"))
  (ensure-directory! destination-directory)
  (when (file-exists? (server-context-log-path context))
    (copy-file (server-context-log-path context)
               (build-path destination-directory (string-append phase ".log"))
               #t)))

(define (upload-with-log! context study-id study-path cwd log-path)
  (call-with-output-file log-path
    #:exists 'truncate/replace
    (lambda (out)
      (upload-study! context study-id study-path
                     #:cwd cwd
                     #:stdout out
                     #:stderr 'stdout))))

(define (expand-environment-value value harness-config-path server-url)
  (define first
    (string-replace (~a value)
                    "${HARNESS_CONFIG_DIR}"
                    harness-config-path))
  (string-replace first "${CONGAME_URL}" server-url))

(define (prepare-harness-config! harness harness-directory run-root)
  (define config-source-name (hash-ref harness 'config_directory #f))
  (and config-source-name
       (let ([config-destination (build-path run-root "harness-config")])
         (copy-tree! (build-path harness-directory config-source-name)
                     config-destination)
         config-destination)))

(define (container-server-url context)
  (string-replace (server-context-url context)
                  "127.0.0.1"
                  "host.docker.internal"))

(define (host-id flag)
  (define-values (_result output _error)
    (run-command/capture "id" (list flag)))
  (string-trim output))

(define (docker-mount source destination #:read-only? [read-only? #f])
  (format "type=bind,src=~a,dst=~a~a"
          (path-string source)
          destination
          (if read-only? ",readonly" "")))

(define (docker-environment harness config-destination server-url)
  (define config-path (if config-destination "/harness-config" ""))
  (define overrides
    (for/hasheq ([(key value) (in-hash (hash-ref harness 'environment #hasheq()))])
      (values key
              (expand-environment-value value
                                        config-path
                                        server-url))))
  (define limits (jref harness 'limits))
  (hash-set*
   overrides
   'HOME "/home/bench"
   'PLTUSERHOME "/home/bench"
   'CONGAME_URL server-url
   'CONGAME_CLI_NO_OPEN "1"
   'LLM_BENCH_SERVER_URL server-url
   'LLM_BENCH_MAX_COST_USD (~a (hash-ref limits 'max_cost_usd 0))))

(define (required-host-environment harness)
  (define declared-environment (hash-ref harness 'environment #hasheq()))
  (for/list ([name (in-list (hash-ref harness 'environment_from_host null))])
    (unless (and (string? name)
                 (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" name))
      (error 'run-harness!
             "invalid environment_from_host entry: ~s"
             name))
    (when (hash-has-key? declared-environment (string->symbol name))
      (error 'run-harness!
             "environment variable is both declared and forwarded from the host: ~a"
             name))
    (unless (let ([value (getenv name)])
              (and value (not (string=? value ""))))
      (error 'run-harness!
             "required host environment variable is not set: ~a"
             name))
    name))

(define (configure-container-cli! context docker-config server-url)
  (define credential-environment
    (make-environment
     (hasheq 'CONGAME_BENCH_CLI_KEY (server-context-api-key context)
             'CONGAME_BENCH_CLI_URL server-url)))
  (define expression
    (string-append
     "(begin (require racket/file) "
     "(put-preferences '(congame-cli:key) "
     "(list (cons (getenv \"CONGAME_BENCH_CLI_URL\") "
     "(getenv \"CONGAME_BENCH_CLI_KEY\")))))"))
  (run-command/capture
   "docker"
   (list "run" "--rm"
         "--network" "none"
         "--read-only"
         "--cap-drop" "ALL"
         "--security-opt" "no-new-privileges"
         "--user" (format "~a:~a" (host-id "-u") (host-id "-g"))
         "--tmpfs" "/tmp:rw,nosuid,nodev,size=16m"
         "--mount" (docker-mount (server-context-home context) "/home/bench")
         "--env" "HOME=/home/bench"
         "--env" "PLTUSERHOME=/home/bench"
         ;; Name-only forwarding keeps the generated credential out of the
         ;; Docker command line and any command-failure diagnostics.
         "--env" "CONGAME_BENCH_CLI_KEY"
         "--env" "CONGAME_BENCH_CLI_URL"
         (jref docker-config 'image)
         "racket" "-e" expression)
   #:environment credential-environment))

(define (run-harness! harness harness-directory workspace run-root context result-path run-id
                      #:stream-view? [stream-view? #f])
  (define script (build-path harness-directory (jref harness 'script)))
  (unless (file-exists? script)
    (error 'run-harness! "harness script does not exist: ~a" script))
  (define limits (jref harness 'limits))
  (define docker-config (jref harness 'docker))
  (define container-name (string-append "congame-llm-bench-" (short-slug run-id)))
  (define server-url (container-server-url context))
  (define config-destination
    (prepare-harness-config! harness harness-directory run-root))
  (configure-container-cli! context docker-config server-url)
  (define environment
    (docker-environment harness config-destination server-url))
  (define host-environment
    (required-host-environment harness))
  (define docker-args
    (append
     (list "run" "--rm"
           "--name" container-name
           "--init"
           "--network" "bridge"
           "--add-host" "host.docker.internal:host-gateway"
           "--read-only"
           "--cap-drop" "ALL"
           "--security-opt" "no-new-privileges"
           "--pids-limit" (~a (hash-ref docker-config 'pids_limit 256))
           "--cpus" (~a (hash-ref docker-config 'cpus 4))
           "--memory" (~a (hash-ref docker-config 'memory "4g"))
           "--user" (format "~a:~a" (host-id "-u") (host-id "-g"))
           "--tmpfs" "/tmp:rw,nosuid,nodev,size=1g"
           "--tmpfs" "/var/tmp:rw,nosuid,nodev,size=1g"
           "--mount" (docker-mount workspace "/workspace")
           "--mount" (docker-mount (build-path run-root "reference")
                                    "/reference"
                                    #:read-only? #t)
           "--mount" (docker-mount harness-directory "/harness" #:read-only? #t)
           "--mount" (docker-mount (server-context-home context) "/home/bench")
           "--workdir" "/workspace")
     (if config-destination
         (list "--mount" (docker-mount config-destination "/harness-config"))
         null)
     (append*
      (for/list ([(key value) (in-hash environment)])
        (list "--env" (format "~a=~a" key value))))
     (append*
      (for/list ([name (in-list host-environment)])
        ;; Passing only the name keeps the secret out of Docker's command-line
        ;; arguments; Docker copies the value from its own environment.
        (list "--env" name)))
     (list (jref docker-config 'image)
           "/bin/bash"
           (string-append "/harness/" (jref harness 'script)))))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file (build-path result-path "stdout.log")
        #:exists 'truncate/replace
        (lambda (stdout)
          (call-with-output-file (build-path result-path "stderr.log")
            #:exists 'truncate/replace
            (lambda (stderr)
              (define live-stderr
                (combine-output stderr (current-error-port)))
              (define (run-with-output live-stdout)
                (run-command
                 "docker"
                 docker-args
                 #:cwd workspace
                 #:stdout live-stdout
                 #:stderr live-stderr
                 #:timeout-seconds (jref limits 'wall_seconds)
                 #:grace-seconds (hash-ref limits 'termination_grace_seconds 10)))
              (if stream-view?
                  (let-values ([(view-input view-output) (make-pipe)])
                    (let ([view-thread
                           (thread
                            (lambda ()
                              (with-handlers
                                  ([exn:fail?
                                    (lambda (e)
                                      (eprintf "stream view disabled: ~a\n"
                                               (exn-message e))
                                      ;; Keep draining the pipe so a terminal-rendering
                                      ;; failure cannot block the harness, and preserve
                                      ;; the remainder of the raw log.
                                      (copy-port view-input stdout))])
                                (render-pi-stream view-input
                                                  (current-output-port)
                                                  #:copy-to stdout))))])
                      (dynamic-wind
                        void
                        (lambda () (run-with-output view-output))
                        (lambda ()
                          (close-output-port view-output)
                          (thread-wait view-thread)
                          (close-input-port view-input)))))
                  (run-with-output
                   (combine-output stdout (current-output-port)))))))))
    (lambda ()
      (run-command/capture "docker" (list "rm" "--force" container-name)
                           #:check? #f))))

(define (run-grading! task task-directory repository-root run-root result-path snapshot run-id)
  (define grade-workspace (build-path run-root "grade-workspace"))
  (apply-snapshot! (build-path task-directory (jref task 'fixture))
                   grade-workspace
                   (workspace-snapshot-patch-path snapshot))
  (define study-id (jref task 'study_id))
  (define study-path (build-path grade-workspace (hash-ref task 'study_path "study.rkt")))
  (define checks-path (build-path result-path "checks"))
  (ensure-directory! checks-path)
  (define upload-result #f)
  (define visible-results null)
  (define hidden-results null)
  (call-with-benchmark-server
   repository-root run-root run-id "grade"
   (lambda (context)
     (dynamic-wind
       void
       (lambda ()
         (set! upload-result
               (upload-with-log!
                context study-id study-path grade-workspace
                (build-path checks-path "upload.log")))
         (when (and (not (command-result-timed-out? upload-result))
                    (zero? (command-result-exit-code upload-result)))
           (create-study-instance! context study-id (short-slug run-id))
           (define check-environment
             (make-environment
              (hasheq
               'CONGAME_URL (server-context-url context)
               'CONGAME_STUDY_ID study-id
               'CONGAME_INSTANCE_SLUG (short-slug run-id)
               'CONGAME_PARTICIPANT_URL
               (format "~a/_anon-login/~a"
                       (server-context-url context)
                       (short-slug run-id)))
              #:base (server-context-environment context)))
           (define timeout (hash-ref task 'check_timeout_seconds 300))
           (set! visible-results
                 (run-checks!
                  (hash-ref task 'visible_checks null)
                  grade-workspace
                  (build-path checks-path "visible")
                  check-environment
                  #:task-directory task-directory
                  #:repository-root repository-root
                  #:timeout-seconds timeout))
           (set! hidden-results
                 (run-checks!
                  (hash-ref task 'hidden_checks null)
                  grade-workspace
                  (build-path checks-path "hidden")
                  check-environment
                  #:task-directory task-directory
                  #:repository-root repository-root
                  #:timeout-seconds timeout))))
       (lambda () (copy-server-log! context result-path "grade")))))
  (define upload-json
    (hasheq
     'exit_code (and upload-result (command-result-exit-code upload-result))
     'timed_out (and upload-result (command-result-timed-out? upload-result))
     'duration_seconds (and upload-result (command-result-duration-seconds upload-result))
     'passed (and upload-result
                  (not (command-result-timed-out? upload-result))
                  (zero? (command-result-exit-code upload-result)))
     'log "upload.log"))
  (define summary
    (hasheq 'upload upload-json
            'visible visible-results
            'hidden hidden-results))
  (write-result-json! result-path "checks/summary.json" summary)
  summary)

(define (run-one! benchmark-root repository-root task-id harness-id repetition
                  #:commit? [commit? #t]
                  #:stream-view? [stream-view? #f])
  (define task (load-task task-id))
  (define harness (load-harness harness-id))
  ;; Fail before creating result records or starting host services when a
  ;; required host environment value is unavailable.
  (required-host-environment harness)
  (define task-directory (resolve-benchmark-path "tasks" task-id))
  (define harness-directory (resolve-benchmark-path "harnesses" harness-id))
  (define run-id (make-run-id task-id harness-id repetition))
  ;; Capture source state before creating the untracked result directory.
  (define repository-commit (git-head repository-root))
  (define repository-dirty? (not (string=? (git-status repository-root) "")))
  (define result-path (create-result-directory! benchmark-root run-id))
  (define run-root (make-temporary-file "congame-llm-bench-~a" 'directory))
  (define harness-result #f)
  (define snapshot #f)
  (define checks #f)
  (define finished? #f)
  (define started-at (timestamp))
  (define (finish-result! status [error-message #f])
    (unless finished?
      (set! finished? #t)
      (write-result-json!
       result-path
       "outcome.json"
       (hasheq
        'status status
        'error error-message
        'exit_code (and harness-result (command-result-exit-code harness-result))
        'timed_out (and harness-result (command-result-timed-out? harness-result))
        'duration_seconds (and harness-result (command-result-duration-seconds harness-result))
        'changed_paths (if snapshot (workspace-snapshot-changed-paths snapshot) null)
        'scope_violations (if snapshot (workspace-snapshot-violations snapshot) null)
        'checks checks
        'finished_at (timestamp)))
      (write-pending-grade! result-path)
      (when commit?
        (commit-result! repository-root result-path
                        (format "llm-bench: record ~a" run-id)))))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (finish-result! "infrastructure_error" (exn-message e))
          (delete-directory/files run-root)
          (raise e))])
    (define prompt-path (build-path task-directory (jref task 'prompt)))
    (write-result-json!
     result-path
     "manifest.json"
     (hasheq
      'schema_version 1
      'run_id run-id
      'started_at started-at
      'task_id task-id
      'task_version (jref task 'version)
      'harness_id harness-id
      'repetition repetition
      'repository_commit repository-commit
      'repository_dirty repository-dirty?
      'prompt_sha1 (file-sha1 prompt-path)
      'task task
      'harness harness))
    (define instance-slug (short-slug run-id))
    (define workspace #f)
    (call-with-benchmark-server
     repository-root run-root run-id "agent"
     (lambda (context)
       (dynamic-wind
         void
         (lambda ()
           (define starter-path
             (build-path task-directory (jref task 'starter_study)))
           (define setup-upload
             (upload-with-log!
              context (jref task 'study_id) starter-path task-directory
              (build-path result-path "setup-upload.log")))
           (unless (and (not (command-result-timed-out? setup-upload))
                        (zero? (command-result-exit-code setup-upload)))
             (error 'run-one!
                    "failed to upload the task's starter study (exit ~a, timeout ~a):\n~a"
                    (command-result-exit-code setup-upload)
                    (command-result-timed-out? setup-upload)
                    (file->string (build-path result-path "setup-upload.log"))))
           (create-study-instance! context (jref task 'study_id) instance-slug)
           (set! workspace
                 (prepare-workspace!
                  repository-root run-root task task-directory
                  #:server-url (container-server-url context)
                  #:instance-slug instance-slug))
           (copy-file (build-path workspace "TASK.md")
                      (build-path result-path "TASK.md") #t)
           (set! harness-result
                 (run-harness! harness harness-directory workspace run-root context result-path
                               run-id
                               #:stream-view? stream-view?))
           (set! snapshot
                 (snapshot-workspace!
                  workspace result-path (jref task 'allowed_paths))))
         (lambda () (copy-server-log! context result-path "agent")))))
    (set! checks
          (run-grading! task task-directory repository-root run-root result-path
                        snapshot run-id))
    (finish-result!
     (cond
       [(command-result-timed-out? harness-result) "timed_out"]
       [(zero? (command-result-exit-code harness-result)) "completed"]
       [else "harness_error"]))
    (delete-directory/files run-root)
    run-id))

(define (run-suite! benchmark-root repository-root suite-id
                    #:commit? [commit? #t]
                    #:stream-view? [stream-view? #f])
  (define suite (load-suite suite-id))
  (define repetitions (hash-ref suite 'repetitions 1))
  (for*/list ([task-id (in-list (jref suite 'tasks))]
              [harness-id (in-list (jref suite 'harnesses))]
              [repetition (in-range 1 (add1 repetitions))])
    (run-one! benchmark-root repository-root task-id harness-id repetition
              #:commit? commit?
              #:stream-view? stream-view?)))
