#lang racket/gui/easy

(require racket/class
         racket/file
         (prefix-in gui: racket/gui/base)
         racket/list
         racket/string
         "cost.rkt"
         "gui-common.rkt"
         "results.rkt"
         "review-model.rkt"
         "run-cli.rkt"
         "util.rkt")

(provide open-grader)

(define (open-grader root repository-root run-id #:commit? [commit? #t])
  (define path (result-directory root run-id))
  (define manifest (read-json-file (build-path path "manifest.json")))
  (define rubric-id (hash-ref (hash-ref manifest 'task) 'rubric))
  (define rubric (read-json-file (build-path root "rubrics" (string-append rubric-id ".json"))))
  (define axes (hash-ref rubric 'axes))
  (define minimum (hash-ref (hash-ref rubric 'scale) 'minimum))
  (define maximum (hash-ref (hash-ref rubric 'scale) 'maximum))
  (define grade-path (build-path path "grade.json"))
  (define previous
    (if (file-exists? grade-path)
        (read-json-file grade-path)
        #hasheq()))
  (define prior-axes (hash-ref previous 'axes #hasheq()))
  (define scores (make-hasheq))
  (define notes (make-hasheq))
  (for ([axis (in-list axes)])
    (define key (string->symbol (hash-ref axis 'id)))
    (define prior (hash-ref prior-axes key #hasheq()))
    (hash-set! scores key (hash-ref prior 'score #f))
    (hash-set! notes key (hash-ref prior 'notes "")))
  (define evidence (review-evidence path))
  (define cache (make-hash))
  (define (evidence-text item)
    (hash-ref! cache item (lambda () (read-evidence path item))))
  (define/obs @step 0)
  (define/obs @evidence "Run summary")
  (define/obs @reviewer (hash-ref previous 'grader (lambda () (default-reviewer root))))
  (define/obs @overall (hash-ref previous 'overall_notes ""))
  (define/obs @status "")
  ;; Disable editable controls individually: the installed gui-easy panel
  ;; updater calls `enabled` instead of the native `enable` method.
  (define/obs @busy? #f)
  (define dirty? #f)
  (define last-step (add1 (length axes)))
  (define (move! step)
    (obs-set! @evidence
              (cond
                [(zero? step) "Run summary"]
                [(= step last-step) "Run summary"]
                [else
                 (suggested-evidence (hash-ref (list-ref axes (sub1 step)) 'id)
                                     evidence
                                     #:study-path
                                     (hash-ref (hash-ref manifest 'task) 'study_path "study.rkt"))]))
    (obs-set! @step step))
  (define (save!)
    (cond
      [(not (grade-complete? axes scores minimum maximum))
       (obs-set! @status "Choose a valid score for every criterion before saving.")]
      [(string=? (string-trim (obs-peek @reviewer)) "")
       (obs-set! @status "Enter a reviewer name before saving.")]
      [else
       (define grade
         (hasheq
          'status "complete"
          'rubric rubric-id
          'graded_at (timestamp)
          'grader (string-trim (obs-peek @reviewer))
          'axes (for/hasheq ([axis (in-list axes)])
                  (define key (string->symbol (hash-ref axis 'id)))
                  (values key (hasheq 'score (hash-ref scores key)
                                      'notes (hash-ref notes key))))
          'overall_notes (obs-peek @overall)))
       (obs-set! @busy? #t)
       (obs-set! @status "Saving…")
       (thread
        (lambda ()
          (define failure
            (with-handlers ([exn:fail? exn-message])
              (define source (make-temporary-file "llm-grade-~a.json"))
              (dynamic-wind
                void
                (lambda ()
                  (write-json-file source grade)
                  (record-grade! root repository-root run-id source #:commit? commit?))
                (lambda () (delete-file source)))
              #f))
          (gui:queue-callback
           (lambda ()
             (obs-set! @busy? #f)
             (unless failure
               (set! dirty? #f))
             (obs-set! @status
                       (or failure (if commit? "Grade saved and committed." "Grade saved.")))))))]))
  (define (step-view step)
    (cond
      [(zero? step)
       (vpanel #:alignment '(left top)
               #:spacing 10
               (text "The task" #:font heading-font)
               (reader (evidence-text "TASK.md") #:height 300)
               (text "Read the requirements, then review each criterion." #:color muted)
               (text (if (equal? (hash-ref previous 'status #f) "complete")
                         "Editing your saved review."
                         (format "New review · ~a criteria" (length axes)))))]
      [(= step last-step)
       (vpanel #:alignment '(left top)
               #:spacing 10
               (text "Your review" #:font heading-font)
               (reader (string-join (for/list ([axis (in-list axes)])
                                      (define key (string->symbol (hash-ref axis 'id)))
                                      (format "~a: ~a\n~a"
                                              (hash-ref axis 'title (hash-ref axis 'id))
                                              (or (hash-ref scores key) "Not scored")
                                              (hash-ref notes key)))
                                    "\n\n"))
               (input @reviewer
                      #:label "Reviewer"
                      #:enabled? (obs-map @busy? not)
                      (lambda (event value)
                        (when (eq? event 'input)
                          (set! dirty? #t)
                          (obs-set! @reviewer value))))
               (input @overall
                      #:label "Overall notes (optional)"
                      #:enabled? (obs-map @busy? not)
                      #:style '(multiple)
                      #:min-size '(300 90)
                      #:stretch '(#t #t)
                      (lambda (event value)
                        (when (eq? event 'input)
                          (set! dirty? #t)
                          (obs-set! @overall value)))))]
      [else
       (define axis (list-ref axes (sub1 step)))
       (define key (string->symbol (hash-ref axis 'id)))
       (define/obs @score (hash-ref scores key))
       (define/obs @notes (hash-ref notes key))
       (vpanel #:alignment '(left top)
               #:spacing 10
               (text (hash-ref axis 'title (hash-ref axis 'id)) #:font heading-font)
               (reader (hash-ref axis 'description "Compare with the task requirements.")
                       #:height 65
                       #:stretch '(#t #f))
               (hpanel #:stretch '(#t #f)
                       #:alignment '(left center)
                       (text "Your score" #:font section-font)
                       (spacer)
                       (text (obs-map @score
                                      (lambda (value)
                                        (if value
                                            (format "~a / ~a" value maximum)
                                            "Choose a score")))
                             #:font score-font))
               (slider (obs-map @score
                                (lambda (value) (inexact->exact (round (* 10 (or value minimum))))))
                       (lambda (value)
                         (set! dirty? #t)
                         (define score (/ value 10.0))
                         (obs-set! @score score)
                         (hash-set! scores key score))
                       #:enabled? (obs-map @busy? not)
                       #:min-value (* 10 minimum)
                       #:max-value (* 10 maximum)
                       #:style '(horizontal plain)
                       #:stretch '(#t #f))
               (hpanel #:stretch '(#t #f)
                       (text (format "~a · Missing" minimum) #:color muted)
                       (spacer)
                       (text (format "~a · Complete" maximum) #:color muted))
               (text "How to judge this" #:font section-font)
               (reader (string-join (hash-ref axis 'anchors null) "\n\n") #:height 130)
               (text "Notes (optional)" #:font section-font)
               (input @notes
                      #:enabled? (obs-map @busy? not)
                      #:style '(multiple)
                      #:min-size '(300 80)
                      #:stretch '(#t #t)
                      (lambda (event value)
                        (when (eq? event 'input)
                          (set! dirty? #t)
                          (obs-set! @notes value)
                          (hash-set! notes key value)))))]))
  (define (window-mixin %)
    (class %
      (super-new)
      (define/augment (can-close?)
        (and (not (obs-peek @busy?))
             (or (not dirty?)
                 (= 1
                    (gui:message-box/custom "Unsaved review"
                                            "Discard your unsaved changes?"
                                            "Discard"
                                            "Keep reviewing"
                                            #f
                                            this
                                            '(caution default=2))))))))
  (define step-labels
    (append '("Overview")
            (for/list ([axis (in-list axes)]
                       [index (in-naturals 1)])
              (format "~a. ~a" index (hash-ref axis 'title (hash-ref axis 'id))))
            '("Finish review")))
  (render
   (window
    #:title (format "Review · ~a" (hash-ref manifest 'task_id))
    #:size '(1100 760)
    #:mixin window-mixin
    (vpanel
     #:margin '(20 18)
     #:spacing 16
     (hpanel
      #:stretch '(#t #f)
      #:alignment '(left center)
      (vpanel #:alignment '(left top)
              #:spacing 4
              (text "Review run" #:font heading-font)
              (text (format "~a · ~a" (hash-ref manifest 'task_id) (hash-ref manifest 'harness_id))
                    #:color muted)
              (text (format "Cost: ~a" (cost-summary path)) #:color muted))
      (spacer)
      (choice step-labels
              (lambda (label) (move! (index-of step-labels label)))
              #:selection (obs-map @step (lambda (step) (list-ref step-labels step)))
              #:enabled? (obs-map @busy? not)))
     (hpanel #:spacing 22
             (vpanel #:alignment '(left top)
                     #:spacing 10
                     (text "Evidence" #:font section-font)
                     (choice evidence
                             (lambda (value) (obs-set! @evidence value))
                             #:selection @evidence
                             #:stretch '(#t #f)
                             #:choice->label
                             (lambda (value) (string-replace value "review/files/" "Submission · ")))
                     (observable-view @evidence
                                      (lambda (item)
                                        (reader (evidence-text item)
                                                #:height 430
                                                #:code? #t
                                                #:highlight? (regexp-match? #rx"[.](rkt|ss|scm)$"
                                                                            item)))))
             (vpanel (observable-view @step step-view)))
     (if-view (obs-map @status non-empty-string?)
              (reader @status #:height 40 #:stretch '(#t #f))
              (hpanel #:stretch '(#t #f)))
     (hpanel
      #:stretch '(#t #f)
      #:alignment '(left center)
      (button "Back"
              (lambda () (move! (sub1 (obs-peek @step))))
              #:enabled?
              (obs-combine (lambda (step busy?) (and (positive? step) (not busy?))) @step @busy?))
      (text (obs-map @step (lambda (step) (format "~a of ~a" (add1 step) (add1 last-step))))
            #:color muted)
      (spacer)
      (if-view (obs-map @step (lambda (step) (= step last-step)))
               (button (if commit? "Save and commit" "Save review")
                       save!
                       #:style '(border)
                       #:enabled? (obs-map @busy? not))
               (button (obs-map @step (lambda (step) (if (zero? step) "Start review →" "Continue →")))
                       (lambda () (move! (add1 (obs-peek @step))))
                       #:style '(border)
                       #:enabled? (obs-map @busy? not))))))))
