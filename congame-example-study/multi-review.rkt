#lang racket/base

(require component
         congame/components/bot
         congame/components/export
         congame/components/formular
         congame/components/study
         congame-web/components/identity
         (prefix-in upload: congame-web/components/upload)
         gregor
         koyo/haml
         koyo/job
         (only-in forms ok err)
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/port
         racket/random
         sentry
         threading
         web-server/http
         "mail.rkt"
         "tools.rkt")

;; FIXME: refactor submit+review-study once we have used it some -- there is
;; quite some duplication of code across the main and the admin studies,
;; suggesting it can be done better.

;; design:
;; * the entire class is part of the same group
;; * the size of the group is predeterimened (eg. 10 people total)
;; * first, everybody has to submit a submission before anybody can make progress
;; * everybody is assigned to review 2 other random people from the group

(provide
 submit+review-pdf
 submit+review-pdf/intro-R
 submit+review-pdf/intro-R-exercises
 submit+review-pdf/beliefs
 submit+review-research-ideas)

;; FIXME: the worst-case complexity here is really bad.  There must be
;; a way to do this without drawing randomly and retrying on failure.
(define (assign-reviewers pids [n 2])
  (put/instance 'number-of-other-reviewers n)
  (when (<= (length pids) n)
    (raise-arguments-error 'assign-reviewers "pids must be > n " "pids" pids "n" n))
  (define (help)
    (for/fold ([assignments (hash)]
               [pids pids]
               #:result assignments)
              ([pid (in-list pids)])
      (define eligible (remove pid pids))
      #:break (null? eligible)
      (define choice (random-ref eligible))
      (values
       (hash-update assignments pid (λ (vs) (cons choice vs)) null)
       (remove choice pids))))
  (let loop ([assignments (hash)]
             [i n])
    (cond
      [(zero? i) assignments]
      [else
       (define next-i (sub1 i))
       (define assignments* (help))
       (define merged
         (for/hash ([(pid l) (in-hash assignments*)])
           (values pid (append l (hash-ref assignments pid null)))))
       (define ok?
         ; `help` can return incorrect assignments, which we check:
         ; - a person is not assigned anyone, because they would have to be
         ;   assigned themselves
         ; - a person is assigned the same submission two or more times
         (and (= (length (hash-values merged)) (length pids))
              (for/and ([l (in-hash-values merged)])
                (= (length (remove-duplicates l)) (- n next-i)))))
       (if ok?
           (loop merged next-i)
           (loop assignments i))])))

(define (matchmake #:self-review? [self-review? #t])
  (define (add-self-to-reviewers assignments)
    (for/hash ([(reviewer assignments) (in-hash assignments)])
      (values reviewer (append assignments (list reviewer)))))
  (with-study-transaction
    (define submissions (get/instance 'submissions (hash)))
    (cond
      [#;(or (= (hash-count submissions) class-size)
             (get/instance 'admin-triggers-assignments #f))
       (get/instance 'admin-triggers-assignments #f)
       (define assignments (get/instance 'assignments #f))
       (cond
         [assignments #t]
         [else
          (define participant-ids (hash-keys submissions))
          (define assigned-reviewers (assign-reviewers participant-ids))
          (put/instance 'assignments
                        (if (not self-review?)
                            assigned-reviewers
                            (add-self-to-reviewers assigned-reviewers)))
          #t])]
      [else #f])))

; JS script for reloading page
;
;          #<<SCRIPT
;setTimeout(function() {
;  document.location.reload();
;}, 1000);
;SCRIPT

(define (lobby)
  (define review-phase? (equal? (get/instance 'phase) 'review))
  (if review-phase?
      (page
       (haml
        (.container
         (:h1 "Review Phase has started")
         (:p "You can now go on to review submissions assigned to you.")
         (button
          (lambda ()
            (define all-assignments (get/instance 'assignments))
            (define participant-assignments (hash-ref all-assignments (current-participant-id)))
            (put 'assignments participant-assignments)
            (put 'n-assignments (length participant-assignments))
            (put 'n-reviewed-assignments 0)
            (define all-submissions (get/instance 'submissions))
            (define participant-submissions
              (for/hash ([a participant-assignments])
                (values a (hash-ref all-submissions a))))
            (put 'submissions participant-submissions))
          "Continue."))))
      (page
       (haml
        (.container
         (:h1 "Waiting for Review Phase to Start")
         (:p "You have reached the end of the submission phase. Come back once the review phase has started.")
         (button
          void
          "Replace your submissions"
          #:to-step-id 'submit))))))

(define/contract ((final compute-scores final-page))
  ; FIXME: compute-score should return a hash of submission -> score. This should be checked upon creation of the study with a helpful error message.
  (-> (-> (hash/c any/c any/c)) (-> any) any)
  ; FIXME: compute-scores gets called on every refresh, which can be costly.
  ; Recompute scores only if triggered in the admin interface or based on a
  ; timed job.
  (define scores (compute-scores))
  (put 'scores scores)
  (final-page))

(define (update-submissions)
  (with-study-transaction
    (define submission (get 'submission))
    (define submissions (get/instance 'submissions (hash)))
    (define updated-submissions (hash-set submissions (current-participant-id) submission))
    (put/instance 'submissions updated-submissions))
  (skip))

(define (update-reviews)
  (with-study-transaction
    (define participant-reviews (get 'participant-reviews))
    (define reviews (get/instance 'reviews '()))
    (define updated-reviews (append reviews participant-reviews))
    (put/instance 'reviews updated-reviews)))

(define-job (send-review-phase-started-email participant-email study-name)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (sentry-capture-exception! e)
                     (raise e))])
    (define mailer (system-ref 'mailer))
    (mailer-send-next-phase-started-email mailer participant-email study-name)))

(define (send-review-phase-email participant-email study-name)
  (unless (current-user-bot?)
    (schedule-at
     (now/moment)
     (send-review-phase-started-email participant-email study-name))))

(define (send-review-phase-notifications instance-id)
  (define participant-ids
    (parameterize ([current-study-stack '(*root*)])
      (hash-keys (get/instance 'assignments (hash)))))
  (for ([p participant-ids])
    (send-review-phase-email (participant-email p) (current-study-instance-name))))

(define (default-submissions-interface)
  (haml
   (:div
    (:h2 "Submissions")
    (:ul
     ,@(for/list ([submitter-id (hash-keys (get/instance 'submissions (hash)))])
         (haml
          (:li (~a submitter-id))))))))

(define (default-reviews-interface)
  (haml
   (:div
    (:h3 "Assigned Reviews")
    (:ul
     ,@(for/list ([(submitter-id reviewer-ids) (in-hash (get/instance 'assignments (hash)))])
         (haml
          (:li (format "Submitter ~a with reviewers: " submitter-id)
               (:ul
                ,@(for/list ([r (in-list reviewer-ids)])
                    (haml
                     (:li (~a r))))))))))))

(define (phase-button when-phase next-phase text)
  (parameterize ([current-study-stack '(*root*)])
    (cond [(equal? (get/instance 'phase) when-phase)
           (haml
            (:div
             (button
              (λ ()
                (parameterize ([current-study-stack '(*root*)])
                  (put/instance 'phase next-phase)))
              text)))]
          [else
           (haml
            (:div))])))

(define ((admin-interface-handler
          #:submissions-interface (submissions-interface default-submissions-interface)
          #:reviews-interface (reviews-interface default-reviews-interface)
          #:self-review? [self-review? #t]))
  (define phase-buttons
    (parameterize ([current-study-stack '(*root*)])
      (haml
       (:div
        (phase-button 'init 'submit "Go to Submit Phase")
        (if (get/instance 'assignments #f)
            (phase-button 'submit 'review "Go to Review Phase")
            `(div))
        (phase-button 'review 'over "Close Review Phase")
        (phase-button 'review 'submit "Re-Open Submit Phase")
        (phase-button 'over 'review "Re-Open Review Phase")))))

  (page
   (parameterize ([current-study-stack '(*root*)])
     (haml
      (.container
       (:h1 "Admin Interface")

       (submissions-interface)
       (reviews-interface)

       (cond [(member (get/instance 'phase) '(review done))
              (haml
               (:div
                (:p "Submission phase is over -- cannot reassign reviews")))]

             [(and (equal? (get/instance 'phase) 'submit)
                   (not (get/instance 'assignments #f)))
              (button
               (λ ()
                 (parameterize ([current-study-stack '(*root*)])
                   (put/instance 'admin-triggers-assignments #t)
                   (matchmake #:self-review? self-review?)))
               "Assign Reviews")]

             [(and (equal? (get/instance 'phase) 'submit)
                   (get/instance 'assignments #f))
              (haml
               (:div
                (:div
                 (button
                  (λ ()
                    (parameterize ([current-study-stack '(*root*)])
                      (put/instance 'admin-triggers-assignments #t)
                      (put/instance 'assignments #f)
                      (matchmake)))
                  "Reassign Reviews"
                  #:to-step-id 'admin))))]

             [else
              (haml (:div))])
       (:div
        (:h4 "Change Phases")
        phase-buttons))))))

(define (default-admin-interface)
  (make-study
   "default-admin-inteface"
   #:provides '()
   #:requires '()
   (list
    (make-step 'admin (admin-interface-handler)))))

(define (default-final-page)
  (page
   (haml
    (.container
     (:h1 "Thank you for participating")
     (:p "You are done.")))))

(define (wait-submit-phase)
  (case (get/instance 'phase 'none)
    [(none init)    (page
                     (haml
                      (.container
                       (:h1 "Submissions are not yet open. Come back later."))))]
    [(submit)       (skip)]
    [(review over)  (page
                     (haml
                      (.container
                       (:h1 "Submissions are closed."))))]))

(define ((wrapper/check-review? s))
  (define phase
    (parameterize ([current-study-stack '(*root*)])
      (get/instance 'phase)))
  (cond [(equal? phase 'review)
         (s)]
        [else
         (displayln "running faily faily...")
         (fail 'not-in-review-phase-anymore)]))

(define (submit+review-study #:submission-study submission-study
                             #:review-study review-study
                             #:submission-key submission-key
                             #:compute-scores [compute-scores (λ () (hash))]
                             #:final-page [final-page default-final-page]
                             #:admin-interface [admin-interface (default-admin-interface)])
  (define (show-next-review)
    (define n (get 'n-reviewed-assignments))
    (page
     (haml
      (.container
       (:h1 (format "Review Next Submission (~a out of ~a)" (add1 n) (get 'n-assignments)))
       (button
        void
        "Go to Review")))))

  (make-study
   "review-study"
   #:requires '()
   #:provides '()
   #:failure-handler (λ (_s e)
                       (case e
                         [(not-in-review-phase-anymore)
                          (update-reviews)
                          'final]
                         [else (fail e)]))
   (list
    (make-step 'start (λ ()
                        (page
                         (haml
                          (.container
                           (:h1 "Submission and Review")
                           (:p "This is the start of the submission and review process.")
                           (button
                            (λ ()
                              (unless (get/instance 'phase #f)
                                (put/instance 'phase 'init)))
                            "Go to Submission"))))))
    (make-step 'check-owner skip
               (λ ()
                 (cond [(current-participant-owner?)
                        'admin-interface]
                       [else
                        'wait-submit-phase])))
    (make-step/study 'admin-interface
                     admin-interface
                     (λ ()
                       'admin-interface))
    (make-step
     'wait-submit-phase
     wait-submit-phase)
    (make-step/study 'submit submission-study
                     #:provide-bindings `([submission ,submission-key]))
    (make-step 'update-submissions update-submissions)
    (make-step 'lobby lobby)
    (make-step 'show-next-review (wrapper/check-review? show-next-review))
    (make-step/study 'next-assignment-reviews
                     (wrap-sub-study review-study wrapper/check-review?)
                     (λ ()
                       (put 'participant-reviews (append (get 'participant-reviews '()) (get 'next-reviews)))
                       (put 'assignments (cdr (get 'assignments)))
                       (put 'n-reviewed-assignments (add1 (get 'n-reviewed-assignments)))
                       (cond [(empty? (get 'assignments))
                              (update-reviews)
                              'final]
                             [else 'show-next-review]))
                     #:require-bindings '((assignments assignments)
                                          (submissions submissions))
                     #:provide-bindings '((next-reviews reviews)))
    (make-step 'final (final compute-scores final-page))
    )))

;; helpers for admin-interface

(define (submit-submissions submit-single-submission (n 2) #:study-name the-study-name)
  ; Elicits `n` submissions
  (define (initialize)
    (put 'n n)
    (put 'submissions '())
    (skip))

  (define (next-or-done/transition)
    (cond [(< (length (get 'submissions))
              (get 'n))
           'submit]
          [else
           (put 'submissions (reverse (get 'submissions)))
           done]))

  (make-study
   the-study-name
   #:requires '()
   #:provides '(submissions)
   (list
    (make-step 'initialize initialize next-or-done/transition)
    (make-step 'submit submit-single-submission next-or-done/transition))))

(define (get-reviews-of-participant)
  (filter (λ (r)
            (equal? (hash-ref r 'submitter-id) (current-participant-id)))
          (get/instance 'reviews)))

(define (get-reviews-by-participant)
  (filter (λ (r)
            (and (hash-has-key? r 'reviewer-id)
                 (equal? (hash-ref r 'reviewer-id) (current-participant-id))))
          (get/instance 'reviews)))

(define (review-submissions review-next-submission)

  (define (initialize-review)
    (define assignments (get 'assignments))
    (define current-assignment (car assignments))
    ; Clear reviews since last review. FIXME: Stateful stuff that is obnoxious
    ; to deal with.
    (put 'reviews '())
    (put 'current-assignment current-assignment)
    (define submissions (get 'submissions))
    (define current-submissions (hash-ref submissions current-assignment))
    (put 'current-submissions current-submissions)
    (put 'n-reviewed-submissions 0)
    (put 'n-total-submissions (length current-submissions))
    (skip))

  (make-study
   "submission-review"
   #:provides '(reviews)
   #:requires '(assignments submissions)
   (list
    (make-step
     'initialize-review
     initialize-review
     (λ ()
       (if (empty? (get 'current-submissions)) done 'review-next-submission)))

    (make-step
     'review-next-submission
     review-next-submission
     (λ ()
       (when (not (empty? (get 'current-submissions)))
         (put 'current-submissions (cdr (get 'current-submissions)))
         (put 'n-reviewed-submissions (add1 (get 'n-reviewed-submissions))))
       (cond [(empty? (get 'current-submissions))
              done]
             [else
              'review-next-submission]))))))

(define (submissions-admin-interface-handler names keys #:self-review? self-review?)

  (define submissions
    (parameterize ([current-study-stack '(*root*)])
      (get/instance 'submissions (hash))))

  (define reviews
    (parameterize ([current-study-stack '(*root*)])
      (sort (get/instance 'reviews '())
            (λ (x y)
              (< (hash-ref x 'reviewer-id)
                 (hash-ref y 'reviewer-id))))))

  (define admin-reviews
    (filter (λ (r)
              (equal? (hash-ref r 'reviewer-id) (current-participant-id)))
            reviews))

  (define reviewed
    (for/hash ([r admin-reviews])
      (values (list (hash-ref r 'submitter-id)
                    (hash-ref r 'submission-id))
              r)))

  (define phase
    (parameterize ([current-study-stack '(*root*)])
      (get/instance 'phase)))
  (define review-phase?
    (equal? phase 'review))

  (define (submissions-interface)
    (haml
     (:div
      (:h3 "Submissions")
      (:table
       (:thead
        (:tr
         (:th "Submitter ID")
         (:th "Submission ID")
         (:th "Submission")
         (:th "Admin has reviewed?")
         (:th "Review Submission")))
       (:tbody
        ,@(for*/list ([(submitter-id ids&submissions) (in-hash submissions)]
                      [id&submission ids&submissions])
            (match-define (hash-table ('submission submission)
                                      ('submission-id submission-id))
              id&submission)
            (haml
             (:tr
              (:td (~a (->jsexpr submitter-id)))
              (:td (~a submission-id))
              (:td (~a (->jsexpr
                        (if (uploaded-file? submission)
                            (uploaded-file-filename submission)
                            submission))))
              (:td (cond [(hash-ref reviewed (list submitter-id submission-id) #f)
                          "Yes"]
                         [else "No"]))
              (:td
               (case phase
                 [(init submit)
                  "Init/Submission phase -- cannot review"]
                 [(review over)
                  (button
                   (λ ()
                     (put 'next-submissions (hash submitter-id (list id&submission)))
                     (put 'next-assignments (list submitter-id)))
                   "Review this Submission"
                   #:to-step-id 'admin-review)]))))))))))

  (define (submissions-reviews-interface)
    (haml
     (:div
      (:table
       (:thead
        (:th "Reviewer ID")
        (:th "Submitter ID")
        (:th "Submission")
        ,@(for/list ([t names])
            (haml
             (:th t)))
        (:th "Comments on Review")
        (:th "Score of Review")
        (:th "Review this Review")
        (:th "Remove this Review"))
       (:tbody
        ,@(for/list ([r reviews])
            (match-define (hash-table ('submission submission)
                                      ('reviewer-id reviewer-id)
                                      ('submission-id submission-id)
                                      ('submitter-id submitter-id))
              r)
            (displayln (format "SUBMISSION: ~a" submission))
            (haml
             (:tr
              (:td (if (equal? reviewer-id (current-participant-id))
                       "Myself"
                       (~a reviewer-id)))
              (:td (~a submitter-id))
              (:td (~a (->jsexpr (if (uploaded-file? submission)
                                     (uploaded-file-filename submission)
                                     submission))))
              ,@(for/list ([k keys])
                  (haml
                   (:td (~a (hash-ref r k)))))
              (:td (~a (hash-ref r 'comments-on-review "")))
              (:td (~a (hash-ref r 'review-score "")))
              (:td
               (case phase
                 [(init submit)
                  "Submission Phase -- cannot review"]
                 [(review over)
                   (button
                    (λ ()
                      (put 'next-submissions (hash reviewer-id (list r)))
                      (put 'next-assignments (list reviewer-id)))
                    (if (hash-has-key? r 'comments-on-review)
                        "Edit Review of Review"
                        "Review the Review")
                    #:to-step-id 'admin-review-review)]))
              (:td
               (button
                (λ ()
                  (parameterize ([current-study-stack '(*root*)])
                    (define new-reviews
                      (remove r reviews))
                    (put/instance 'reviews new-reviews)
                    (put/instance 'removed-reviews
                                  (cons r (get 'removed-reviews '())))))
                "Remove Review"))))))))))

  ((admin-interface-handler #:submissions-interface submissions-interface
                            #:reviews-interface submissions-reviews-interface
                            #:self-review? self-review?)))

(define (submissions-admin-interface
         submissions-admin-handler
         the-study-name
         review-submissions
         review-reviews)

  (define ((review-equal? r1) r2)
    (define (equal-key? k)
      (equal? (hash-ref r1 k) (hash-ref r2 k)))
    (and (equal-key? 'submitter-id)
         (equal-key? 'submission-id)
         (equal-key? 'reviewer-id)))

  (define (review-in-reviews? r rs)
    (findf (review-equal? r) rs))

  (define (update-admin-reviews new-reviews)
    (with-study-transaction
      (parameterize ([current-study-stack '(*root*)])
        (define current-reviews (get/instance 'reviews '()))
        (define rs
          (filter (λ (r)
                    (not (review-in-reviews? r new-reviews)))
                  current-reviews))
        (define updated-reviews
          (append new-reviews rs))
        (put/instance 'reviews updated-reviews))))

  (make-study
   the-study-name
   #:provides '()
   #:requires '()
   (list
    (make-step 'admin submissions-admin-handler (λ () 'admin))
    (make-step/study
     'admin-review
     (review-submissions)
     #:require-bindings '((assignments next-assignments)
                          (submissions next-submissions))
     #:provide-bindings '((next-reviews reviews))
     (λ ()
       (update-admin-reviews (get 'next-reviews))
       'admin))
    (make-step/study
     'admin-review-review
     (review-reviews)
     #:require-bindings '((assignments next-assignments)
                          (submissions next-submissions))
     #:provide-bindings '((next-review-reviews reviews))
     (λ ()
       (update-admin-reviews (get 'next-review-reviews))
       'admin))
    (make-step 'done skip (λ () done)))))

;; SUBMIT+REVIEW-PDF

(define ((submit-single-pdf-submission #:instructions [instructions #f]))
  (page
   (haml
    (.container
     (if instructions instructions (haml (:div "")))
     (formular
      (haml
       (:div
        (#:submission (input-file "Please provide your pdf submission for the assignment" #:validators (list valid-pdf?)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:submission submission)
        (define submission-upload
          (upload-file! submission #:prefix (number->string (current-participant-id))))
        (put 'submissions
             (cons (hash 'submission-id (get 'next-submission-id 0)
                         'submission submission-upload)
                   (get 'submissions)))
        (put 'next-submission-id (add1 (get 'next-submission-id 0)))))))))

(define (submit-pdf-submissions [n 2] #:instructions [instructions #f])
  (submit-submissions
   (submit-single-pdf-submission #:instructions instructions)
   n
   #:study-name "submit-pdf-submission"))

(define (review-next-pdf-handler)
  (define r (car (get 'current-submissions)))
  (match-define (hash-table
                 ('submission submission-upload)
                 ('submission-id submission-id))
    r)
  (page
   (haml
    (.container
     (:h1 (format "Review this PDF ~a (of ~a) for this submitter" (add1 (get 'n-reviewed-submissions)) (get 'n-total-submissions)))
     (:p.submission (file-download/link submission-upload "Download"))
     (:div
      (:h3 "Rubric for PDF")
      (formular
       (haml
        (:div
         (#:how-good-is-the-submission
          (input-number "On a scale from 0 (very bad) to 5 (very good), how good is the submission?"
                        #:min 0 #:max 5))
         (:button.button.next-button ([:type "submit"]) "Submit")))
       (lambda (#:how-good-is-the-submission how-good-is-the-submission)
         (put 'reviews
              (cons
               (~> r
                   (hash-set 'submitter-id (get 'current-assignment))
                   (hash-set 'reviewer-id (current-participant-id))
                   (hash-set 'how-good-is-the-submission how-good-is-the-submission))
               (get 'reviews '()))))))))))

(define (review-R-intro-pdf-handler)
  (define subs (get 'current-submissions))
  (define r (car subs))
  (match-define (hash-table
                 ('submission submission-upload)
                 ('submission-id submission-id))
    r)
  (page
   (haml
    (.container
     (:h1 "Review this PDF")
     (:p.submission (file-download/link submission-upload "Download submission"))
     (:div
      (:h3 "Rubric for PDF")
      (formular
       (haml
        (:div
         (#:genuine-attempt
          (radios
           "Is this assignment a genuine attempt?"
           '(("0" . "(0 points) No assignment submitted, or it contains barely anything different from the default template provided in class.")
             ("1" . "(1 point) Half or more of the exercises were not attempted.")
             ("2" . "(2 points) 75% of all parts were attempted in a meaningful way.")
             ("3" . "(3 points) All parts were attempted in a meaningful way."))))

         (#:genuine-attempt-explanation
          (input-textarea "In one sentence, explain your score whether this submission is a genuine attempt. E.g. \"While all parts were attempted, half of them do little more than restate the exercise and do not solve it.\" Suggest one (not two or five) ways to improve the submissions: pick a single concrete example from the submission and how it could have been improved. E.g. \"You should say why you chose the particular functions you used. In exercise 2, for example, you use both `geom_col` and `geom_bar` without even briefly saying what each of them does."))

         (#:where-got-stuck
          (radios
           "For incomplete problems, did the person highlight clearly why they got stuck?"
           '(("0" . "(0 points) There is no code example included for any of the problems that occurred, no explanation given for why something wasn't achieved, no attempt at resolving issues that the person encountered.")
             ("1" . "(1 point) Whenever a problem was encountered, the author simply states that they couldn't get any further and didn't know how to solve it. They didn't post questions about these issues in the forum (or they did so less than 24 hours before the deadline).")
             ("2" . "(2 points) For most problems, the person states clearly what part of the code they think is the issue, and describe it well enough that you can understand what went wrong. Most of the time, you can replicate the problem.")
             ("3" . "(3 points) Either the person never got stuck. Or for every problem encountered, the person clearly states where things went wrong, what they tried (google searches, Slack questions, R help). You can see what went wrong, even if the diagnosis of the problem may itself be wrong. "))))

         (#:where-got-stuck-explanation
          (input-textarea "In one sentence, explain your score on incomplete problems and how clearly the person highlighted where they got stuck. E.g. \"Whenever you got stuck, you simply seem to have stopped without saying why you stopped or what you tried yet failed to work.\" Suggest one (not two or five) ways to improve the submissions: pick a single concrete example from the submission and how it could have been improved. E.g. \"For exercise 3, it seems that you simply mistyped the variable name (it is missing an underscore '_'). If so, you should have said that you ran the code and what error you hit.\""))

         (#:clear-presentation
          (radios
           "How clear is the writing and presentation? Take into account that an incorrect answer is likely to be less clear and more confusing, but try not to penalize again for this. Thus if the person writes clearly what they did (even if wrong), you should give high score."
           '(("0" . "(0 points) The document is an incomprehensible jumble of things. It is hard to know which graph belongs to what, the goal and intent of the code and graphs is not explained.")
             ("1" . "(1 point) While code and graphs are described, it is quite hard for you to follow what is going on without putting in some effort.")
             ("2" . "(2 points) Most code and graphs are well described, they are well referenced, and you are rarely confused.")
             ("3" . "(3 points) The document is clearly written, graphs and code well described, and the document is as easy to understand as the material allows."))))

         (#:clear-presentation-explanation
          (input-textarea "Explain your score on presentation in one sentence (e.g. \"I had to spend a lot of time to figure out which graph/which chunk of code belonged to which exercise\"). Suggest one (not two or five) ways to improve the submissions: pick a single concrete example from the submission and how it could have been improved. E.g. \"For exercise 4, I suggest to split the code across several lines and to add a comment why you chose the method 'gam' rather than 'lm'.\" No more than 2 sentences."))
         (:button.button.next-button ([:type "submit"]) "Submit")))
       (lambda (#:genuine-attempt genuine-attempt
                #:genuine-attempt-explanation genuine-attempt-explanation
                #:where-got-stuck where-got-stuck
                #:where-got-stuck-explanation where-got-stuck-explanation
                #:clear-presentation clear-presentation
                #:clear-presentation-explanation clear-presentation-explanation)
         (define genuine-attempt/n (string->number genuine-attempt))
         (define where-got-stuck/n (string->number where-got-stuck))
         (define clear-presentation/n (string->number clear-presentation))
         (put 'reviews
              (cons
               (~> r
                   (hash-set 'submitter-id (get 'current-assignment))
                   (hash-set 'reviewer-id (current-participant-id))
                   (hash-set 'genuine-attempt genuine-attempt/n)
                   (hash-set 'genuine-attempt-explanation genuine-attempt-explanation)
                   (hash-set 'where-got-stuck where-got-stuck/n)
                   (hash-set 'where-got-stuck-explanation where-got-stuck-explanation)
                   (hash-set 'clear-presentation clear-presentation/n)
                   (hash-set 'clear-presentation-explanation clear-presentation-explanation)
                   (hash-set 'score (+ genuine-attempt/n where-got-stuck/n clear-presentation/n)))
               (get 'reviews '()))))))))))

(define (display-review/intro-R r)
  (define rid (hash-ref r 'reviewer-id))
  (haml
   (:div
    (:h3 (format "Score and Feedback by ~a ~a: ~a"
                 rid
                 (if (equal? (current-participant-id) rid)
                     "(your self review)"
                     "")
                 (hash-ref r 'score)))
    (:ul
     ,@(for/list ([answer `((genuine-attempt "Genuine Attempt" genuine-attempt-explanation)
                            (where-got-stuck "Highlight where/how you got stuck" where-got-stuck-explanation)
                            (clear-presentation "Clear Presentation" clear-presentation-explanation))])
         (haml
          (haml
           (:li
            (~a (second answer)) ":"
            (:ul
             (:li "Score: " (~a (hash-ref r (first answer))))
             (:li "Explanation/Feedback: " (~a (hash-ref r (third answer)))))))))))))

(define (display-review-scores r)
  (haml
   (:div
    (:h4 (format "You reviewed participant ~a" (hash-ref r 'submitter-id)))
    (if (hash-has-key? r 'review-score)
        (haml
         (:ul
          (:li "Your Score:" (~a (hash-ref r 'review-score "(not reviewed = 0)")))
          (:li "Comments: " (~a (hash-ref r 'comments-on-review "(not reviewed)")))))
        (haml
         (:p "This review was not reviewed."))))))

(define (display-review-scores/intro-R r)
  (haml
   (.review-scores
    (display-review-scores r)

    (:p "Here is the review you provided:")
    (:table
     (:thead
      (:tr
       (:th "Aspect")
       (:th "Score")
       (:th "Explanation")))
     ,@(for/list ([d '(("Was this a genuine attempt?" genuine-attempt genuine-attempt-explanation)
                                       ("Where did person get stuck?" where-got-stuck where-got-stuck-explanation)
                                       ("Clear Presentation" clear-presentation clear-presentation-explanation))])
         (haml
          (:tr
           (:td (first d))
           (:td (~a (hash-ref r (second d) "<Error: Score not Found>")))
           (:td (~a (hash-ref r (third d) "<Error: Explanation not Found>"))))))))))

(define (display-review-scores/beliefs r)
  (define (~display key)
    (define a (hash-ref r key (format "#<Error: ~a not found>" key)))
    (match a
      [#f "No"]
      [#t "Yes"]
      [else (~a a)]))

  (haml
   (.review-scores
    (display-review-scores r)
    (:p "Here is the review you provided:")
    (:table
     (:thead
      (:tr
       (:th "One Page Limit")
       (:th "How clear was question")
       (:th "Quality")
       (:th "Feedback")))
     (:tr
      (:td (~display 'one-page-limit))
      (:td (~display 'how-clear-question))
      (:td (~display 'quality))
      (:td (~display 'feedback)))))))

(define (final-page/intro-R)

  (define participant-reviews (get-reviews-of-participant))
  (define reviews-by-participant (get-reviews-by-participant))

  (define reviewer-score
    (for/sum ([r reviews-by-participant])
      (hash-ref r 'review-score 0)))

  (define submission-score
    (+
     (for/fold ([score 0.0]
                [n 0]
                #:result (if (zero? n) 0 (/ score n)))
               ([r participant-reviews])
       (values
        (+ score
           (hash-ref r 'genuine-attempt)
           (hash-ref r 'where-got-stuck)
           (hash-ref r 'clear-presentation))
        (add1 n)))))

  (define total-score
    (+ submission-score reviewer-score))

  (define n-other-reviewers-total (get/instance 'number-of-other-reviewers))
  (define n-other-reviewers-received
    (~> participant-reviews
        (map (λ (r)
               (hash-ref r 'reviewer-id))
             _)
        remove-duplicates
        (remove (current-participant-id) _)
        length))
  (define n-other-reviewers-pending
    (- n-other-reviewers-total n-other-reviewers-received))

  (page
   (haml
    (.container
     (:h1 "You are done")

     (:p "Your participation id is: " (~a (current-participant-id)))

     (:p (format "Total Score for submissions and reviews: ~a (~a reviews from others received, ~a reviews pending)"
                  total-score
                  n-other-reviewers-received
                  n-other-reviewers-pending) " This sums up the score from your submission and from your reviews. See below for the breakdown.")

     (:h2 "Detailed Scores and Feedback on Your Submission")

     ,@(for/list ([r participant-reviews])
         (display-review/intro-R r))

     (:h2 "Bonus/Malus for Quality of Your Reviews: " (~a reviewer-score))

     (:p "In addition to the score you receive for your submission, I (Marc) review the reviews each week. Unless your review is particularly good or bad, you will receive 0 -- otherwise a +1 or -1. If it says 'not reviewed', it means that I did not review your review, not that you did not submit it.")

     ,@(for/list ([r reviews-by-participant])
         (display-review-scores/intro-R r)
         )))))

(define (review-pdf)
  (review-submissions review-next-pdf-handler))

(define (review-pdf/intro-R)
  (review-submissions review-R-intro-pdf-handler))

(define (pdf-admin-interface-handler)
  (submissions-admin-interface-handler '() '() #:self-review? #t))

(define (review-next-review-pdfs)
  (define r (car (get 'current-submissions)))
  (match-define (hash-table
                 ('submission submission-upload)
                 ('reviewer-id reviewer-id)
                 ('submitter-id submitter-id))
    r)
  (page
   (haml
    (.container
     (:h1 "Review this Review")

     (.submission
      (:h3 "Original Submission and Review")
      (:p (:strong "submitter id: ") (~a submitter-id))
      (:p (:strong "Submission: ")
          (file-download/link submission-upload "Download File")))

     (formular
      (haml
       (:div
        (#:comments-on-review
         (input-textarea "Comments on Review:"))
        (#:review-score
         (input-number "Review Score (-1 to +1): "
                       #:min -1 #:max 1))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:comments-on-review comments-on-review
          #:review-score review-score)
        (put 'reviews
             (cons
              (hash-set
               (hash-set r 'comments-on-review comments-on-review)
               'review-score review-score)
              (get 'reviews '())))))))))

(define (review-reviews-pdf)
  (review-submissions review-next-review-pdfs))

(define (pdf-full-admin-interface)
  (submissions-admin-interface
   pdf-admin-interface-handler
   "PDF study"
   review-pdf
   review-reviews-pdf))

(define ((review-next-review display-review))
  (define r (car (get 'current-submissions)))
  (match-define (hash-table
                 ('submission submission-upload)
                 ('reviewer-id reviewer-id)
                 ('submitter-id submitter-id))
    r)
  (displayln (format "REVIEW: ~a" r))
  (page
   (haml
    (.container
     (:h1 "Review this Review")
     (.submission
      (:h3 "Submission")
      (:p (file-download/link (hash-ref r 'submission) "Submitted file")))

     (:h3 "Review")
     (display-review r)

     (:h3 "Feedback on Review")
     (formular
      (haml
       (:div
        (#:comments-on-review
         (input-textarea "Comments on Review:"))
        (#:review-score
         (input-number "Review Score (-1 to +1): "
                       #:min -1 #:max 1))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:comments-on-review comments-on-review
          #:review-score review-score)
        (put 'reviews
             (cons
              (hash-set
               (hash-set r 'comments-on-review comments-on-review)
               'review-score review-score)
              (get 'reviews '())))))))))

(define (review-reviews/intro-R)
  (review-submissions (review-next-review display-review/intro-R)))

(define (pdf-admin-interface/intro-R)
  (submissions-admin-interface
   pdf-admin-interface-handler
   "PDF review for Intro R"
   review-pdf/intro-R
   review-reviews/intro-R))

(define submit+review-pdf
  (submit+review-study #:submission-study (submit-pdf-submissions 2)
                       #:review-study (review-pdf)
                       #:submission-key 'submissions
                       #:admin-interface (pdf-full-admin-interface)))

(define submit+review-pdf/intro-R
  (submit+review-study #:submission-study (submit-pdf-submissions 1)
                       #:review-study (review-pdf/intro-R)
                       #:submission-key 'submissions
                       #:admin-interface (pdf-admin-interface/intro-R)
                       #:final-page final-page/intro-R))

(define (display-rubric/description)
  (haml
   (:div
    (:h3 "Rubric for Research Proposal")
    (:p "The research proposal will be evaluated along the following rouch guidelines. Notice that both a focus on research or on policy are acceptable. A research focus suggests a cleaner, narrower question that can potentially be answered well, but will not necessarily have direct policy implications. Causality, prediction, and understanding play a larger role. A policy focus suggests a more relevant, important, and sizeable effect, but is not necessarily as clean on causality, mechanism, and understanding. The potential impact on policy plays a larger role.")
    (:ul
     (:li "What is the research / policy question? (1-2 sentences)")
     (:li "What (if any) are some relevant papers?")
     (:li "What domain does this apply to? What is a concrete (ideally real-life) example that fits the description? (2-4 sentences)")
     (:li "Describe what you plan on doing. For laboratory or field experiments, explain how you think you could identify the main parameter (and what that parameter is!); for theory, explain the simplest interesting setting you can think of working out.")
     (:li "Why do you care (and why should other economists care)? (2 sentences. Don’t bullshit, say why you really care, not what you think readers want to hear.)")))))

(define (review-pdf-handler/beliefs)
  (define r (car (get 'current-submissions)))
  (match-define (hash-table
                 ('submission submission-upload)
                 ('submission-id submission-id))
    r)
  (page
   (haml
    (.container
     (:h1 "Review this PDF")
     (:p.submission (file-download/link submission-upload "Download submission"))
     (:div
      (display-rubric/description)

      (formular
       (haml
        (:div
         (#:one-page-limit (radios
                            "Does the proposal stay within the 1-page limit without resorting to tiny fonts or similar tricks?"
                            '(("yes" . "Yes")
                              ("no"  . "No"))))
         (#:how-clear-question (input-number "How clearly is the research / policy question articulated from 0 (least clear) to 2 (most clear)?"
                                             #:min 0 #:max 2))
         (#:quality (input-number "What overall score between 0 (lowest) and 7 (highest) would you give this proposal?"
                                  #:min 0 #:max 7))
         (#:feedback (input-textarea "Provide comments for the overall score and give at least one constructive item of feedback: a suggestion how to improve one particular aspect of the proposal."))
         (:button.button.next-button ((:type "submit")) "Submit")))
       (lambda (#:one-page-limit one-page-limit
                #:how-clear-question how-clear-question
                #:quality quality
                #:feedback feedback)
         (define op (string=? "yes" one-page-limit))
         (put 'reviews
              (cons
               (~> r
                   (hash-set 'submitter-id (get 'current-assignment))
                   (hash-set 'reviewer-id (current-participant-id))
                   (hash-set 'one-page-limit op)
                   (hash-set 'how-clear-question how-clear-question)
                   (hash-set 'quality quality)
                   (hash-set 'feedback feedback)
                   (hash-set 'score (+ (if op 1 0) how-clear-question quality)))
               (get 'reviews '()))))))))))

(define (display-review/beliefs r)

  (define rid (hash-ref r 'reviewer-id))
  (haml
   (:div
    (:h3 (format "Score and Feedback by ~a ~a: ~a"
                 rid
                 (if (equal? (current-participant-id) rid)
                     "(your self review)"
                     "")
                 (hash-ref r 'score)))
    (:ul
     ,@(for/list ([answer `((one-page-limit "One Page Limit")
                            (how-clear-question "How clear was the research / policy question")
                            (quality "What was the overall quality of the proposal? (out of 7)")
                            (feedback "Feedback"))])
         (haml
          (haml
           (:li
            (~a (second answer)) ":" (~a (hash-ref r (first answer)))))))))))

(define (final-page/beliefs)

  (define participant-reviews (get-reviews-of-participant))
  (define reviews-by-participant (get-reviews-by-participant))

  (define submission-score
    (for/fold ([score 0.0]
               [n 0]
               #:result (if (zero? n) 0 (/ score n)))
              ([r participant-reviews])
      (values (+ score (hash-ref r 'score))
              (add1 n))))

  (define review-score
    (for/sum ([r reviews-by-participant])
      (hash-ref r 'review-score 0)))

  (define total-score
    (+ submission-score review-score))

  (define n-other-reviewers-total (get/instance 'number-of-other-reviewers))
  (define n-other-reviewers-received
    (~> participant-reviews
        (map (λ (r)
               (hash-ref r 'reviewer-id))
             _)
        remove-duplicates
        (remove (current-participant-id) _)
        length))
  (define n-other-reviewers-pending
    (- n-other-reviewers-total n-other-reviewers-received))

  (page
   (haml
    (.container
     (:h1 "You are done")

     (:p "Your participation number is: " (~a (current-participant-id)))

     (:h4 (format "Total Score: ~a (~a reviews from others received, ~a reviews pending)"
                  total-score
                  n-other-reviewers-received
                  n-other-reviewers-pending))
     (:h3 "Scores and Feedback")

     ,@(for/list ([r participant-reviews])
         (display-review/beliefs r))

     (:h3 "Scores for Reviews")

     ,@(for/list ([r reviews-by-participant])
         (display-review-scores/beliefs r))))))

(define (review-reviews/beliefs)
  (review-submissions (review-next-review display-review/beliefs)))

(define (pdf-admin-interface/beliefs)
  (submissions-admin-interface
   pdf-admin-interface-handler
   "PDF reivew for Research Proposal"
   review-pdf/beliefs
   review-reviews/beliefs))

(define (review-pdf/beliefs)
  (review-submissions review-pdf-handler/beliefs))

(define instructions/beliefs
  (display-rubric/description))

(define submit+review-pdf/beliefs
  (submit+review-study #:submission-study (submit-pdf-submissions 1 #:instructions instructions/beliefs)
                       #:review-study (review-pdf/beliefs)
                       #:submission-key 'submissions
                       #:final-page final-page/beliefs
                       #:admin-interface (pdf-admin-interface/beliefs)))

;;; SUBMIT+REVIEW-RESEARCH-IDEAS

(define (submit-single-research-idea)
  (define i (add1 (length (get 'submissions '()))))
  (define n (get 'n))
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:research-idea (input-textarea (format "Provide research idea ~a of ~a" i n)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:research-idea idea)
        (put 'submissions
             (cons (hash 'submission-id (get 'next-submission-id 0)
                         'submission idea)
                   (get 'submissions)))
        (put 'next-submission-id (add1 (get 'next-submission-id 0)))))))))

(define (submit-research-ideas [n 2])
  (submit-submissions submit-single-research-idea n #:study-name "research-ideas-study"))

(define (review-next-research-idea)
  (define research-ideas-examples-url "https://ceulearning.ceu.edu/mod/resource/view.php?id=405798")
  (match-define (hash-table ('submission submission)
                            ('submission-id submission-id))
    (car (get 'current-submissions)))
  (page
   (haml
    (.container
     (:h1 (format "Review Research Idea ~a (of ~a)" (add1 (get 'n-reviewed-submissions)) (get 'n-total-submissions)))
     (.submission
      (:h3 "Submitted Research Idea")
      (:p submission))

     (:h3 "Rubric for Research Idea")
     (:p "See "(:a ((:href research-ideas-examples-url)) "Examples of Research Ideas") " for details. Only mark an idea as invalid if it cannot be interpreted as a research question OR if it is effectively a duplicate of another research question by this person. Ignore (for now) whether it is interesting or original; whether you like it; whether there is a way to answer it; ... . Those comments you can keep for constructive and kind feedback.")
     (formular
      (haml
       (:div
        (#:valid-research-idea?
         (radios
          "Do you consider this a valid research idea?"
          '(("yes" . "Yes")
            ("no"  . "No"))))
        (#:feedback (input-textarea "Provide a constructive suggestion how to improve this research question / turn it into a valid research question. Remember: be kind."))
        (:button.button.next-button ((:type "submit")) "Submit")))
      (lambda (#:valid-research-idea? valid-research-idea?
               #:feedback feedback)
        (put 'reviews
             (cons
              (hash 'submitter-id           (get 'current-assignment)
                    'reviewer-id            (current-participant-id)
                    'submission             submission
                    'submission-id          submission-id
                    'valid-research-idea?   (string=? valid-research-idea? "yes")
                    'feedback               feedback)
              (get 'reviews '())))))))))

(define (review-next-review-research-ideas)
  (define r (car (get 'current-submissions)))
  (match-define (hash-table ('feedback feedback)
                            ('submission submission)
                            ('reviewer-id reviewer-id)
                            ('submitter-id submitter-id)
                            ('valid-research-idea? valid?))
    r)
  (page
   (haml
    (.container
     (:h1 "Review this Review")

     (.submission
      (:h3 "Original Submission and Review")
      (:p (:strong "Submission: ") submission)
      (:p (:strong "Review: ") feedback))

     (formular
      (haml
       (:div
        (#:comments-on-review
         (input-textarea "Comments on Review:"))
        (#:review-score
         (input-number "Review Score (-1 to +1): "
                       #:min -1 #:max 1))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:comments-on-review comments-on-review
          #:review-score review-score)
        (put 'reviews
             (cons
              (hash-set
               (hash-set r 'comments-on-review comments-on-review)
               'review-score review-score)
              (get 'reviews '())))))))))

(define (review-research-ideas)
  (review-submissions review-next-research-idea))

(define (review-reviews-research-ideas)
  (review-submissions review-next-review-research-ideas))

(define (compute-research-ideas-scores)
  (define (mean los)
    (/ (apply + los) (* 1.0 (length los))))
  (define (score r)
    (if (hash-ref r 'valid-research-idea?) 1 0))
  (define collect-by-idea
    (for/fold ([reviewer-scores (hash)])
              ([r (get-reviews-of-participant)])
      (define research-idea (hash-ref r 'submission))
      (hash-set reviewer-scores
                research-idea
                (cons (score r)
                      (hash-ref reviewer-scores research-idea '())))))
  (define submission-scores
    (for/hash ([(idea scores) (in-hash collect-by-idea)])
    (values idea
            (mean scores))))
  (hash 'total-score        (apply + (hash-values submission-scores))
        'submission-scores  submission-scores))

(define (display-review-scores/research-ideas r)
  (haml
   (.review-scores
    (display-review-scores r)

    (:p "Here is the review you provided:")
    (:table
     (:thead
      (:tr
       (:th "Valid?")
       (:th "Feedback")))
     (:tr
      (:td (match (hash-ref r 'valid-research-idea? "#<Error: not found>")
             [#f "No"]
             [#t "Yes"]
             [else "#<Error: not found>"]))
      (:td (~a (hash-ref r 'feedback "#<Error: not found>"))))))))

(define (research-ideas-final-page)
  (define scores (get 'scores))
  (define participant-reviews (get-reviews-of-participant))
  (define n-other-reviewers-total (get/instance 'number-of-other-reviewers))
  (define n-other-reviewers-received
    (~> participant-reviews
        (map (λ (r)
               (hash-ref r 'reviewer-id))
             _)
        remove-duplicates
        (remove (current-participant-id) _)
        length))
  (define n-other-reviewers-pending
    (- n-other-reviewers-total n-other-reviewers-received))

  (define score-display
    (haml
     (:div
      (:h4 (format "Total Score: ~a (~a reviews received, ~a reviews pending)"
                   (hash-ref scores 'total-score)
                   n-other-reviewers-received
                   n-other-reviewers-pending))

      (:p (format "Your participant id is ~a." (current-participant-id)))

      (:h4 "Score by research idea is:")
      (:ul
       ,@(for/list ([(idea score) (in-hash (hash-ref scores 'submission-scores))])
           (haml
            (:li (format "Research Idea: ~a" idea)
             (:ul
              (:li (format "Score: ~a" (~r score #:precision 2)))))))))))

  (define reviews-by-participant
    (get-reviews-by-participant))

  (define submission-feedback
    (haml
     (:div
      (:h3 (format "Detailed Reviews (~a received, ~a pending)"
                   n-other-reviewers-received
                   (- n-other-reviewers-total n-other-reviewers-received)))
      (:div
       ,@(for/list ([r participant-reviews])
           (define reviewer (hash-ref r 'reviewer-id))
           (define valid? (hash-ref r 'valid-research-idea?))
           (define research-idea (hash-ref r 'submission))
           (define feedback (hash-ref r 'feedback))
           (haml
            (.feedback
             (:p (:strong "Research Idea: ") research-idea)
             (:ul
              (:li "Reviewer: " (~a reviewer))
              (:li "Feedback: " feedback)
              (:li "Valid: " (if valid? "Yes" "No"))))))))))
  (page
   (haml
    (.container
     (:h1 "End of Review Phase")
     (:p "Thanks for completing your reviews.")
     score-display
     submission-feedback

     (:h3 "Scores for Reviews")

     ,@(for/list ([r reviews-by-participant])
         (display-review-scores/research-ideas r))))))

(define (research-ideas-admin-handler)
  (submissions-admin-interface-handler
   '("Valid Idea?" "Feedback")
   '(valid-research-idea? feedback)
   #:self-review? #f))

(define (research-ideas-admin-interface)
  (submissions-admin-interface
   research-ideas-admin-handler
   "research-ideas-admin-interface"
   review-research-ideas
   review-reviews-research-ideas))

;; FIXME: Design study so that the number of research ideas can be configured by
;; the admin after creating a study instance.
;; FIXME: Refactor (e.g. number-of-submissions)
(define submit+review-research-ideas
  (submit+review-study #:submission-study (submit-research-ideas 4)
                       #:review-study (review-research-ideas)
                       #:submission-key 'submissions
                       #:compute-scores compute-research-ideas-scores
                       #:final-page research-ideas-final-page
                       #:admin-interface (research-ideas-admin-interface)))

;;;; R with review of individual exercises
(define (review-R-intro-pdf-handler/exercises)
  ; FIXME: Change this to store exercise points
  (define subs (get 'current-submissions))
  (define r (car subs))
  (match-define (hash-table
                 ('submission submission-upload)
                 ('submission-id submission-id))
    r)
  (define (input-exercise-score n)
    (input-number
     (format "Score on exercise ~a -- from 0 to 5 (see explanation on grades above)" n)
     #:min 0 #:max 5))
  (page
   (haml
    (.container
     (:h1 "Review this PDF")
     (:p.submission (file-download/link submission-upload "Download submission"))
     (:div
      (:h3 "Rubric for PDF")
      (:p "Each exercise should be scored from 0 to 5 and thus counts the same as every other exercise. At the end, the sum of scores gets rescaled to however much the current assignment is worth (15 or 25). The meaning of scores is as follows:")
      (:ul
       (:li "0: Not attempted/Wrong Exercise/Nothing of Value")
       (:li "1: A minor point was achieved or the person at least stated why they got stuck (lack of time does not count).")
       (:li "2: Some progress was made, or the person reports several meaningful attempts that failed (with code examples) that pointed in the right direction and explain why the person got stuck. (Lack of time still does not count.) Alternatively, the person describes how the problem could be approached conceptually and this is genuinely helpful to solve the problem.")
       (:li "3: Same as 2, but with more progress. Alternatively, the person achieved almost the right solution but due to a bug the result is wrong in a major way (i.e. not just a typo or some minor oversight, but it does not work or provide the right output).")
       (:li "4: The solution is close to complete. There is some minor detail missing, a minor bug (typo), some subtlety in forgetting to drop edge cases. Alternatively, the solution is correct and complete, but unclear, confusing, and with poor code.")
       (:li "5: The solution is clear, correct, and complete."))
      (formular
       (haml
        (:div
         (#:exercise1
          (input-exercise-score 1))
         (#:exercise1-explanation
          (input-textarea "Explain your score for exercise 1 briefly."))
         (#:exercise2
          (input-exercise-score 2))
         (#:exercise2-explanation
          (input-textarea "Explain your score for exercise 2 briefly."))
         (#:exercise3
          (input-exercise-score 3))
         (#:exercise3-explanation
          (input-textarea "Explain your score for exercise 3 briefly."))
         (#:exercise4
          (input-exercise-score 4))
         (#:exercise4-explanation
          (input-textarea "Explain your score for exercise 4 briefly."))
         (#:exercise5
          (input-exercise-score 5))
         (#:exercise5-explanation
          (input-textarea "Explain your score for exercise 5 briefly."))
         (#:exercise6
          (input-exercise-score 6))
         (#:exercise6-explanation
          (input-textarea "Explain your score for exercise 6 briefly."))
         (:button.button.next-button ([:type "submit"]) "Submit")
         (display-admin-cancel-button)))
       (lambda (#:exercise1 exercise1
                #:exercise2 exercise2
                #:exercise3 exercise3
                #:exercise4 exercise4
                #:exercise5 exercise5
                #:exercise6 exercise6
                #:exercise1-explanation exercise1-explanation
                #:exercise2-explanation exercise2-explanation
                #:exercise3-explanation exercise3-explanation
                #:exercise4-explanation exercise4-explanation
                #:exercise5-explanation exercise5-explanation
                #:exercise6-explanation exercise6-explanation)
         (define (make-exercise name score expl)
           (hash 'name name
                 'score score
                 'explanation expl))
         (define exercises
           (list
            (make-exercise "Exercise 1" exercise1 exercise1-explanation)
            (make-exercise "Exercise 2" exercise2 exercise2-explanation)
            (make-exercise "Exercise 3" exercise3 exercise3-explanation)
            (make-exercise "Exercise 4" exercise4 exercise4-explanation)
            (make-exercise "Exercise 5" exercise5 exercise5-explanation)
            (make-exercise "Exercise 6" exercise6 exercise6-explanation)))
         (put 'reviews
              (cons
               (~> r
                   (hash-set 'submitter-id (get 'current-assignment))
                   (hash-set 'reviewer-id (current-participant-id))
                   (hash-set 'exercises exercises))
               (get 'reviews '()))))))))))

(define (review-pdf/intro-R-exercises)
  (review-submissions review-R-intro-pdf-handler/exercises))

(define (display-review-of-exercises r)
  (haml
   (:ul
    ,@(for/list ([exercise (hash-ref r 'exercises '())])
        (haml
         (haml
          (:li
           (hash-ref exercise 'name) ":"
           (:ul
            (:li "Score: " (~a (hash-ref exercise 'score "#<Error: not found>")))
            (:li "Explanation/Feedback: " (~a (hash-ref exercise 'explanation "#<Error: not found>")))))))))))

(define (display-review-scores/intro-R-exercises r)
  (haml
   (.review-scores
    (display-review-scores r)

    (:p "Here is the review you provided:")
    (display-review-of-exercises r))))

(define (display-review/intro-R-exercises r)
  (define rid (hash-ref r 'reviewer-id))
  (haml
   (:div
    (:h3 (format "Scores and Feedback by ~a ~a"
                 rid
                 (if (equal? (current-participant-id) rid)
                     "(your self review)"
                     "")))
    (display-review-of-exercises r))))

(define (final-page/intro-R-exercises)
  ; FIXME: Change this to display the exercises and score based on exercises

  (define participant-reviews (get-reviews-of-participant))
  (define reviews-by-participant (get-reviews-by-participant))

  (define reviewer-score
    (for/sum ([r reviews-by-participant])
      (hash-ref r 'review-score 0)))

  (define (exercises-score r)
    (define exs (hash-ref r 'exercises '()))
    (for/sum ([ex exs])
      (hash-ref ex 'score 0)))
  (define submission-score
    (+
     (for/fold ([score 0.0]
                [n 0]
                #:result (if (zero? n) 0 (/ score n)))
               ([r participant-reviews])
       (values
        (+ score (exercises-score r))
        (add1 n)))))

  (define total-score
    (+ submission-score reviewer-score))

  (define n-other-reviewers-total (get/instance 'number-of-other-reviewers))
  (define n-other-reviewers-received
    (~> participant-reviews
        (map (λ (r)
               (hash-ref r 'reviewer-id))
             _)
        remove-duplicates
        (remove (current-participant-id) _)
        length))
  (define n-other-reviewers-pending
    (- n-other-reviewers-total n-other-reviewers-received))

  (page
   (haml
    (.container
     (:h1 "You are done")

     (:p "Your participation id is: " (~a (current-participant-id)))

     (:p (format "Total Score for submissions and reviews: ~a (~a reviews from others received, ~a reviews pending)"
                  total-score
                  n-other-reviewers-received
                  n-other-reviewers-pending) " This sums up the score from your submission and from your reviews. See below for the breakdown.")

     (:h2 "Detailed Scores and Feedback on Your Submission")

     ,@(for/list ([r participant-reviews])
         (display-review/intro-R-exercises r))

     (:h2 "Bonus/Malus for Quality of Your Reviews: " (~a reviewer-score))

     (:p "In addition to the score you receive for your submission, I (Marc) review the reviews each week. Unless your review is particularly good or bad, you will receive 0 -- otherwise a +1 or -1. If it says 'not reviewed', it means that I did not review your review, not that you did not submit it.")

     ,@(for/list ([r reviews-by-participant])
         (display-review-scores/intro-R-exercises r))))))

(define (review-reviews/intro-R-exercises)
  (review-submissions (review-next-review display-review/intro-R-exercises)))

(define (pdf-admin-interface/intro-R-exercises)
  (submissions-admin-interface
   pdf-admin-interface-handler
   "PDF review for Intro R"
   review-pdf/intro-R-exercises
   review-reviews/intro-R-exercises))

(define submit+review-pdf/intro-R-exercises
  (submit+review-study #:submission-study (submit-pdf-submissions 1)
                       #:review-study (review-pdf/intro-R-exercises)
                       #:submission-key 'submissions
                       #:admin-interface (pdf-admin-interface/intro-R-exercises)
                       #:final-page final-page/intro-R-exercises))
