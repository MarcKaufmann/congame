#lang racket/gui/easy

(require racket/string
         "gui-common.rkt"
         "harness-definition.rkt"
         "wizard-gui.rkt")

(provide open-harness-wizard)

(define (open-harness-wizard root [id ""] #:resume [resume-path #f])
  (define harnesses (build-path root "harnesses"))
  (define templates
    (if (directory-exists? harnesses)
        (sort
         (for/list ([entry (in-list (directory-list harnesses))]
                    #:when (file-exists? (build-path harnesses entry "config.json")))
           (path->string entry))
         string<?)
        null))
  (define/obs @template "Start from scratch")
  (define/obs @template-error "")
  (define (step-view step @draft @editable? update! field picker)
    (case step
      [(0)
       (vpanel
        #:alignment '(left top) #:spacing 14
        (text "Define the harness" #:font heading-font)
        (text "A harness runs an agent inside a Docker image against TASK.md." #:color muted)
        (choice (cons "Start from scratch" templates)
                (lambda (template)
                  (with-handlers ([exn:fail? (lambda (e) (obs-set! @template-error (exn-message e)))])
                    (define draft-id (hash-ref (obs-peek @draft) 'id))
                    (define draft
                      (if (equal? template "Start from scratch")
                          (default-harness-draft draft-id)
                          (harness-template-draft (build-path harnesses template) draft-id)))
                    (for ([(key value) (in-hash draft)]) (update! key value))
                    (obs-set! @template template)
                    (obs-set! @template-error "")))
                #:label "Template" #:selection @template #:enabled? @editable?)
        (text "Selecting a template replaces the settings below; the original is unchanged." #:color muted)
        (if-view (obs-map @template-error non-empty-string?)
                 (reader @template-error #:height 60)
                 (hpanel #:stretch '(#t #f)))
        (field 'id "Harness ID · lowercase letters, digits, and hyphens")
        (field 'image "Docker image")
        (text "Use an existing image with your agent and Congame CLI installed." #:color muted))]
      [(1)
       (vpanel
        #:alignment '(left top) #:spacing 12
        (text "Agent command" #:font heading-font)
        (text "run.sh runs in /workspace. Read TASK.md and exit when the agent finishes." #:color muted)
        (text "The runner invokes Bash. Standard output and errors are captured automatically." #:color muted)
        (field 'script "run.sh" #:height 350))]
      [(2)
       (vpanel
        #:alignment '(left top) #:spacing 10
        (text "Resources and environment" #:font heading-font)
        (picker 'resources "Resource folder · optional" #t)
        (field 'config_directory "Writable configuration folder · relative to resources, optional")
        (text "Resources mount at /harness. The configuration folder is copied for each run." #:color muted)
        (field 'environment "Environment · JSON object of string values" #:height 90)
        (text "Use ${HARNESS_CONFIG_DIR} and ${CONGAME_URL} in values." #:color muted)
        (field 'environment_from_host "Credentials from host · variable names, one per line" #:height 80)
        (text "For example, OPENROUTER_API_KEY. Supply its value when running the benchmark." #:color muted))]
      [(3)
       (vpanel
        #:alignment '(left top) #:spacing 8
        (text "Limits and labels" #:font heading-font)
        (hpanel (field 'cpus "CPUs") (field 'memory "Memory · e.g. 4g"))
        (hpanel (field 'pids "Process limit") (field 'wall_seconds "Time limit · seconds"))
        (hpanel (field 'grace_seconds "Shutdown grace · seconds") (field 'max_cost_usd "Cost limit · USD, 0 for none"))
        (text "The runner enforces resource and time limits. The harness must enforce the cost limit." #:color muted)
        (field 'labels "Labels · JSON object of string values" #:height 100)
        (text "Record provider, model, and thinking level. Use harness: pi only for Pi JSON logs." #:color muted))]
      [(4)
       (define draft (obs-peek @draft))
       (vpanel
        #:alignment '(left top) #:spacing 12
        (text "Review and create" #:font heading-font)
        (text (format "harnesses/~a/" (hash-ref draft 'id)) #:font section-font)
        (reader
         (format "Image: ~a\nCPUs: ~a · Memory: ~a · Processes: ~a\nTime: ~as · Grace: ~as · Cost limit: $~a\n\nHost variables:\n~a\n\nEnvironment:\n~a\n\nResources: ~a\nConfiguration folder: ~a\nLabels: ~a\n\n~a"
                 (hash-ref draft 'image) (hash-ref draft 'cpus) (hash-ref draft 'memory)
                 (hash-ref draft 'pids) (hash-ref draft 'wall_seconds) (hash-ref draft 'grace_seconds)
                 (hash-ref draft 'max_cost_usd) (hash-ref draft 'environment_from_host)
                 (hash-ref draft 'environment) (hash-ref draft 'resources)
                 (hash-ref draft 'config_directory) (hash-ref draft 'labels) (hash-ref draft 'script))
         #:height 350)
        (text "Creates config.json, executable run.sh, and copies resources. Images are not built or pulled." #:color muted))]))
  (open-definition-wizard root "harness" (default-harness-draft id)
                          '("Basics" "Command" "Resources" "Limits" "Review")
                          step-view validate-harness! create-harness! #:resume resume-path))
