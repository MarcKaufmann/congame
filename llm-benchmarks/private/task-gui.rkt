#lang racket/gui/easy

(require racket/class
         (prefix-in gui: racket/gui/base)
         racket/path
         racket/string
         "gui-common.rkt"
         "task-definition.rkt"
         "task-draft.rkt"
         "util.rkt")

(provide open-task-wizard)

(define (open-task-wizard root [id ""] #:resume [resume-path #f])
  (define rubrics
    (for/list ([file (in-list (directory-list (build-path root "rubrics")))]
               #:when (regexp-match? #rx"[.]json$" (path->string file))
               #:do [(define rubric (read-json-file (build-path root "rubrics" file)))]
               #:when (and (hash-has-key? rubric 'scale) (hash-has-key? rubric 'axes)))
      (hash-ref rubric 'id)))
  (when (null? rubrics)
    (error 'new-task "No rubrics are available."))
  (define default-draft
    (hasheq 'id id 'title "" 'study_id "benchmark-study" 'prompt ""
            'fixture_directory "" 'starter_file "" 'checks_directory ""
            'allowed_paths "study.rkt\nexercise.rkt\nassets"
            'visible_checks "[\"raco\", \"make\", \"study.rkt\"]"
            'hidden_checks "" 'timeout "300"
            'rubric (if (member "study-v1" rubrics) "study-v1" (car rubrics))))
  (define initial-saved (and resume-path (load-task-draft resume-path)))
  (define (merge-draft saved)
    (for/fold ([draft default-draft]) ([(key value) (in-hash (hash-ref saved 'draft))])
      (hash-set draft key value)))
  (define/obs @draft (if initial-saved (merge-draft initial-saved) default-draft))
  (define/obs @step (if initial-saved (hash-ref initial-saved 'step) 0))
  (define/obs @draft-path resume-path)
  (define/obs @status "")
  (define/obs @busy? #f)
  (define/obs @created? #f)
  (define @editable? (obs-combine (lambda (busy? created?) (not (or busy? created?))) @busy? @created?))
  (define dirty? #f)
  (define steps '("Basics" "Prompt" "Starting files" "Checks" "Review"))
  (define (update! key value)
    (set! dirty? #t)
    (obs-update! @draft (lambda (draft) (hash-set draft key value))))
  (define (save-draft! #:as? [as? #f])
    (with-handlers ([exn:fail? (lambda (e) (obs-set! @status (exn-message e)) #f)])
      (define current-path (obs-peek @draft-path))
      (define drafts (task-drafts-root root))
      (define directory
        (cond
          [current-path (path-only (path->complete-path current-path))]
          [(directory-exists? drafts) drafts]
          [else root]))
      (define path
        (if (and current-path (not as?))
            current-path
            (gui:put-file "Save task draft" #f directory
                          (if current-path (file-name-from-path current-path) "draft.json")
                          "json")))
      (and path
           (begin
             (save-task-draft! root (obs-peek @draft) (obs-peek @step) #:path path)
             (obs-set! @draft-path path)
             (set! dirty? #f)
             (obs-set! @status (format "Draft saved: ~a" path))
             #t))))
  (define (open-draft!)
    (when (or (not dirty?)
              (= 1 (gui:message-box/custom
                    "Open task draft" "Discard unsaved changes and open another draft?"
                    "Open draft" "Keep editing" #f #f '(caution default=2))))
      (with-handlers ([exn:fail? (lambda (e) (obs-set! @status (exn-message e)))])
        (define directory (task-drafts-root root))
        (define path (gui:get-file "Open task draft" #f
                                   (and (directory-exists? directory) directory)))
        (when path
          (define saved (load-task-draft path))
          (obs-set! @draft (merge-draft saved))
          (obs-set! @step (hash-ref saved 'step))
          (obs-set! @draft-path path)
          (set! dirty? #f)
          (obs-set! @status "Draft opened.")))))
  (define (field key label #:height [height #f])
    (vpanel
     #:alignment '(left top) #:spacing 5 #:stretch (list #t (and height #t))
     (text label #:font section-font)
     (input (obs-map @draft (lambda (draft) (hash-ref draft key)))
            (lambda (event value)
              (when (eq? event 'input) (update! key value)))
            #:enabled? @editable?
            #:style (if height '(multiple) '(single))
            #:min-size (list 400 height)
            #:stretch (list #t (and height #t)))))
  (define (picker key label directory?)
    (hpanel
     #:stretch '(#t #f) #:spacing 8
     (field key label)
     (button "Browse…"
             (lambda ()
               (define selected
                 (if directory? (gui:get-directory label) (gui:get-file label)))
               (when selected (update! key (path->string selected))))
             #:enabled? @editable?)))
  (define (move! step)
    (with-handlers ([exn:fail? (lambda (e) (obs-set! @status (exn-message e)))])
      (when (= step 4) (validate-task! root (obs-peek @draft)))
      (obs-set! @status "")
      (obs-set! @step step)))
  (define (create!)
    (define draft (obs-peek @draft))
    (obs-set! @busy? #t)
    (obs-set! @status "Creating task…")
    (thread
     (lambda ()
       (define result
         (with-handlers ([exn:fail? values])
           (create-task! root draft)))
       (gui:queue-callback
        (lambda ()
          (obs-set! @busy? #f)
          (cond
            [(exn:fail? result) (obs-set! @status (exn-message result))]
            [else
             (set! dirty? #f)
             (obs-set! @created? #t)
             (obs-set! @status
                       (format "Created ~a\nRun with: raco congame-llm-bench run-task --harness <id> ~a"
                               result (hash-ref draft 'id)))]))))))
  (define (step-view step)
    (case step
      [(0)
       (vpanel
        #:alignment '(left top) #:spacing 14
        (text "Define the task" #:font heading-font)
        (text "A task is a prompt, starting files, and independent checks." #:color muted)
        (field 'id "Task ID · lowercase letters, digits, and hyphens")
        (field 'title "Title")
        (field 'study_id "Exported study ID")
        (text "The starter and submission must export this study." #:color muted)
        (choice rubrics (lambda (value) (update! 'rubric value))
                #:label "Grading rubric" #:enabled? @editable?
                #:selection (obs-map @draft (lambda (draft) (hash-ref draft 'rubric)))))]
      [(1)
       (vpanel
        #:alignment '(left top) #:spacing 12
        (text "What should the agent build?" #:font heading-font)
        (text "Describe required behavior, constraints, and how success is judged." #:color muted)
        (text "The runner adds server details and CLI instructions automatically." #:color muted)
        (field 'prompt "Task prompt · Markdown" #:height 320))]
      [(2)
       (vpanel
        #:alignment '(left top) #:spacing 10
        (text "Starting files" #:font heading-font)
        (text "Leave paths blank to generate a minimal Conscript study." #:color muted)
        (picker 'fixture_directory "Agent workspace folder · must contain study.rkt" #t)
        (picker 'starter_file "Starter study · uploaded before the agent begins" #f)
        (text "A custom starter must compile and export the study ID. It is not copied into the workspace." #:color muted)
        (field 'allowed_paths "Files and directories the agent may change · one per line" #:height 120)
        (text "Conscript source, docs, and examples are included as read-only references." #:color muted))]
      [(3)
       (vpanel
        #:alignment '(left top) #:spacing 10
        (text "Verification" #:font heading-font)
        (text "One command per line, as a JSON array. Commands run without a shell." #:color muted)
        (text "Example: [\"raco\", \"make\", \"study.rkt\"]" #:color muted)
        (field 'visible_checks "Visible checks" #:height 85)
        (field 'hidden_checks "Hidden checks · not exposed to the agent" #:height 85)
        (picker 'checks_directory "Check scripts folder · optional" #t)
        (text "Refer to copied scripts as ${TASK_DIR}/checks/<file>." #:color muted)
        (field 'timeout "Timeout per check · seconds"))]
      [(4)
       (define draft (obs-peek @draft))
       (vpanel
        #:alignment '(left top) #:spacing 12
        (text "Review and create" #:font heading-font)
        (text (format "tasks/~a/" (hash-ref draft 'id)) #:font section-font)
        (text "Creates config.json, prompt.md, fixture/, starter/, and optional checks/." #:color muted)
        (text "Custom source files are copied as supplied; compilation is checked when the task runs." #:color muted)
        (reader
         (string-append
          (format "~a\n\n~a\n\n" (hash-ref draft 'title) (hash-ref draft 'prompt))
          (format "Study export: ~a\nRubric: ~a\nAllowed paths:\n~a\n\nVisible checks:\n~a\n\nHidden checks:\n~a"
                  (hash-ref draft 'study_id) (hash-ref draft 'rubric) (hash-ref draft 'allowed_paths)
                  (hash-ref draft 'visible_checks) (hash-ref draft 'hidden_checks)))
         #:height 300)
        (text "The task is saved locally. Suites and Git commits are left to you." #:color muted))]))
  (define (window-mixin %)
    (class %
      (super-new)
      (define/augment (can-close?)
        (and (not (obs-peek @busy?))
             (or (not dirty?)
                 (case (gui:message-box/custom
                        "Unsaved task" "Save this draft to resume it later?"
                        "Save draft" "Discard" "Keep editing" this '(caution default=1))
                   [(1) (save-draft!)]
                   [(2) #t]
                   [else #f]))))))
  (render
   (window
    #:title "New benchmark task" #:size '(850 720) #:mixin window-mixin
    (menu-bar
     (menu
      "&File"
      (menu-item (obs-map @draft-path (lambda (path) (if path "&Save" "&Save...")))
                 (lambda () (save-draft!) (void))
                 #:enabled? @editable?
                 #:shortcut (if (eq? (system-type 'os) 'macosx) '(cmd #\s) '(ctl #\s)))
      (menu-item "Save &As..." (lambda () (save-draft! #:as? #t) (void))
                 #:enabled? @editable?
                 #:shortcut (if (eq? (system-type 'os) 'macosx) '(cmd shift #\s) '(ctl shift #\s)))
      (menu-item "&Open..." open-draft!
                 #:enabled? @editable?
                 #:shortcut (if (eq? (system-type 'os) 'macosx) '(cmd #\o) '(ctl #\o)))))
    (vpanel
     #:margin '(20 18) #:spacing 16
     (hpanel
      #:stretch '(#t #f)
      (text "New task" #:font heading-font)
      (spacer)
      (text (obs-map @step (lambda (step) (format "~a / 5 · ~a" (add1 step) (list-ref steps step))))
            #:color muted))
     (observable-view @step step-view)
     (if-view (obs-map @status non-empty-string?)
              (reader @status #:height 60 #:stretch '(#t #f))
              (hpanel #:stretch '(#t #f)))
     (hpanel
      #:stretch '(#t #f)
      (button "Back" (lambda () (move! (sub1 (obs-peek @step))))
              #:enabled? (obs-combine (lambda (step editable?) (and (positive? step) editable?)) @step @editable?))
      (spacer)
      (if-view (obs-map @step (lambda (step) (= step 4)))
               (button "Create task" create! #:style '(border) #:enabled? @editable?)
               (button "Continue →" (lambda () (move! (add1 (obs-peek @step))))
                       #:style '(border) #:enabled? @editable?)))))))
