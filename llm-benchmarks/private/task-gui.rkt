#lang racket/gui/easy

(require "gui-common.rkt"
         "task-definition.rkt"
         "util.rkt"
         "wizard-gui.rkt")

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
  (define (step-view step @draft @editable? update! field picker)
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
  (open-definition-wizard root "task" default-draft
                          '("Basics" "Prompt" "Starting files" "Checks" "Review")
                          step-view validate-task! create-task! #:resume resume-path))
