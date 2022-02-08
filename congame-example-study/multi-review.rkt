#lang racket/base

(require component
         congame/components/bot
         congame/components/formular
         congame/components/study
         congame/components/export
         gregor
         koyo/haml
         koyo/job
         (only-in forms ok err)
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/random
         sentry
         web-server/http
         "mail.rkt")

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
 submit+review-research-ideas)

(define class-size 3)

;; FIXME: the worst-case complexity here is really bad.  There must be
;; a way to do this without drawing randomly and retrying on failure.
(define (assign-reviewers pids [n 2])
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
         (for/and ([l (in-hash-values merged)])
           (= (length (remove-duplicates l)) (- n next-i))))
       (if ok?
           (loop merged next-i)
           (loop assignments i))])))

(define (matchmake #:self-review? [self-review? #t])
  (define (add-self-to-reviewers assignments)
    (for/hash ([(reviewer assignments) (in-hash assignments)])
      (values reviewer (cons reviewer assignments))))
  (with-study-transaction
    (define submissions (get/instance 'submissions (hash)))
    (cond
      [(or (= (hash-count submissions) class-size)
           (get/instance 'admin-triggers-assignments #f))
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
  (matchmake)
  (define review-phase? (get/instance 'start-review-phase #f))
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
         (:p "You have reached the end of the submission phase. Come back once the review phase has started."))))))

(define/contract ((final compute-scores final-page))
  ; FIXME: compute-score should return a hash of participant-id -> score. This should be checked upon creation of the study with a helpful error message.
  (-> (-> (hash/c integer? number?)) (-> any) any)
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

(define (default-admin-interface)
  (define (admin)
    (page
     (parameterize ([current-study-stack '(*root*)])
       (haml
        (.container
         (:h1 "Default Admin Interface")

         (:h2 "Submissions")

         (:ul
          ,@(for/list ([submitter-id (hash-keys (get/instance 'submissions (hash)))])
              (haml
               (:li (~a submitter-id)))))

         (:h3 "Assigned Reviews")

         (:ul
          ,@(for/list ([(submitter-id reviewer-ids) (in-hash (get/instance 'assignments (hash)))])
              (haml
               (:li (format "Submitter ~a with reviewers: " submitter-id)
                   (:ul
                    ,@(for/list ([r (in-list reviewer-ids)])
                        (haml
                         (:li (~a r)))))))))

         (cond [(get/instance 'start-review-phase #f)
                (haml
                 (:div
                  (:p "Review Phase has started -- cannot reassign reviews")

                  (:div
                   (button
                    (λ ()
                      (parameterize ([current-study-stack '(*root*)])
                        (put/instance 'start-review-phase #f)))
                    "Go back to pre Review"))
                  ))]

               [(not (get/instance 'assignments #f))
                (button
                 (λ ()
                   (parameterize ([current-study-stack '(*root*)])
                     (put/instance 'admin-triggers-assignments #t)
                     (matchmake)))
                 "Assign Reviews")]

               [else
                (haml
                 (:div
                  (:div
                   (button
                    (λ ()
                      (parameterize ([current-study-stack '(*root*)])
                        (put/instance 'admin-triggers-assignments #t)
                        (put/instance 'assignments #f)
                        (matchmake)))
                    "Reassign Reviews"))

                  (:div
                   (button
                    (λ ()
                      (parameterize ([current-study-stack '(*root*)])
                        (put/instance 'start-review-phase #t)
                        (send-review-phase-notifications (current-study-instance-id))))
                    "Start Review Phase"))))]))))))

  (make-study
   "default-admin-interface"
   #:provides '()
   #:requires '()
   (list
    (make-step 'admin admin (λ () 'admin)))))

(define (default-final-page)
  (page
   (haml
    (.container
     (:h1 "Thank you for participating")
     (:p "You are done.")))))

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
   (list
    (make-step 'start (λ () (page
                             (haml
                              (.container
                               (:h1 "Submission and Review")
                               (:p "This is the start of the submission and review process.")
                               (button
                                void
                                "Go to Submission"))))))
    (make-step 'check-owner skip
     (λ ()
       (cond [(current-participant-owner?)
              'admin-interface]
             [else
              'submit])))
    (make-step/study 'admin-interface
                     admin-interface
                     (λ ()
                       'admin-interface))
    (make-step/study 'submit submission-study
                     #:provide-bindings `([submission ,submission-key]))
    (make-step 'update-submissions update-submissions)
    (make-step 'lobby lobby)
    (make-step 'show-next-review show-next-review)
    (make-step/study 'next-assignment-reviews
                     review-study
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

;; SUBMIT+REVIEW-PDF

(define (valid-pdf? b)
  (if  (and (binding:file? b)
            (regexp-match? #rx#"^%PDF-" (binding:file-content b)))
       (ok b)
       (err "the file must be a PDF")))

(define (provide-pdf-submission)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:submission (input-file "Please provide a study submission" #:validators (list valid-pdf?)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:submission submission)
        (put 'submission submission)))))))

(define provide-pdf-submission/study
  (make-study
   "provide-pdf-submission"
   #:requires '()
   #:provides '(submission)
   (list
    (make-step 'provide-pdf-submission provide-pdf-submission))))

(define (review-pdf-handler)
  (define assignments (get 'assignments))
  (define submissions (get 'submissions))
  (define submission-file (hash-ref submissions (car assignments))) ; FIXME: Treats file like a value, which works for now.
  (page
   (haml
    (.container
     (:h1 "Review this Submission")
     (:p.submission
      (:a
       ([:href ((current-embed/url)
                (lambda (_req)
                  (response/output
                   #:headers (list
                              (or (headers-assq* #"content-type" (binding:file-headers submission-file))
                                  (make-header #"content-type" "application/octet-stream"))
                              (make-header #"content-disposition" (string->bytes/utf-8 (format "attachment; filename=\"~a\"" (binding:file-filename submission-file)))))
                   (lambda (out)
                     (write-bytes (binding:file-content submission-file) out)))))])
       "Download"))
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
         (put 'review (hash
                       'submitter-id (car assignments)
                       'reviewer-id (current-participant-id)
                       'submission submission-file
                       'how-good-is-the-submission how-good-is-the-submission)))))))))

(define review-pdf
  (make-study
   "review-pdf"
   #:provides '()
   #:requires '(assignments submissions)

   (list
    (make-step
     'review-pdf
     review-pdf-handler))))

(define submit+review-pdf
  (submit+review-study #:submission-study provide-pdf-submission/study
                       #:review-study review-pdf
                       #:submission-key 'submission))

;;; SUBMIT+REVIEW-RESEARCH-IDEAS

(define (submit-research-ideas [n 2])
  ; Elicits `n` research ideas
  (define (initialize)
    (put 'n n)
    (put 'research-ideas '())
    (skip))

  (define (submit-research-idea)
    (page
     (haml
      (.container
       (formular
        (haml
         (:div
          (#:research-idea (input-text "Provide a research idea"))
          (:button.button.next-button ([:type "submit"]) "Submit")))
        (lambda (#:research-idea idea)
          (put 'research-ideas
               (cons idea (get 'research-ideas)))))))))

  (define (next-or-done/transition)
    (cond [(< (length (get 'research-ideas))
              (get 'n))
           'submit-research-idea]
          [else
           (put 'research-ideas (reverse (get 'research-ideas)))
           done]))

  (make-study
   "research-ideas-study"
   #:requires '()
   #:provides '(research-ideas)
   (list
    (make-step 'initialize initialize next-or-done/transition)
    (make-step 'submit-research-idea submit-research-idea next-or-done/transition))))

(define (review-next-research-idea)
  (define research-ideas-rubric-url "https://trichotomy.xyz")
  (define next-research-idea (car (get 'current-submissions)))
  (page
   (haml
    (.container
     (:h1 (format "Review Research Idea ~a (of ~a)" (add1 (get 'n-reviewed-submissions)) (get 'n-total-submissions)))
     (.submission
      (:h3 "Submitted Research Idea")
      (:p next-research-idea))

     (:h3 "Rubric for Research Idea")
     (:p "See "(:a ((:href research-ideas-rubric-url)) "Explanations on Research Ideas") " for details.")
     (formular
      (haml
       (:div
        (#:valid-research-idea?
         (radios
          "Do you consider this a valid research idea?"
          '(("yes" . "Yes")
            ("no"  . "No"))))
        (#:feedback (input-textarea "Provide a constructive suggestion how to improve this research question / turn it into a valid research question."))
        (:button.button.next-button ((:type "submit")) "Submit")))
      (lambda (#:valid-research-idea? valid-research-idea?
               #:feedback feedback)
        (put 'reviews
             (cons
              (hash 'submitter-id           (get 'current-assignment)
                    'reviewer-id            (current-participant-id)
                    'research-idea          next-research-idea
                    'valid-research-idea?   (string=? valid-research-idea? "yes")
                    'feedback               feedback)
              (get 'reviews '())))))))))

(define (review-next-review)
  (define r (car (get 'current-submissions)))
  (match-define (hash-table ('feedback feedback)
                            ('research-idea research-idea)
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
      (:p (:strong "Submission: ") research-idea)
      (:p (:strong "Review: ") feedback))

     (formular
      (haml
       (:div
        (#:comments-on-review
         (input-textarea "Comments on Review:"))
        (#:review-score
         (input-number "Review Score (-5 to +5): "
                       #:min -5 #:max 5))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:comments-on-review comments-on-review
          #:review-score review-score)
        (put 'reviews
             (cons
              (hash-set
               (hash-set r 'comments-on-review comments-on-review)
               'review-score review-score)
              (get 'reviews)))))))))

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
       (put 'current-submissions (cdr (get 'current-submissions)))
       (put 'n-reviewed-submissions (add1 (get 'n-reviewed-submissions)))
       (cond [(empty? (get 'current-submissions))
              done]
             [else
              'review-next-submission]))))))

(define (review-research-ideas)
  (review-submissions review-next-research-idea))

(define (review-reviews)
  (review-submissions review-next-review))

(define (get-reviews-of-participant)
  (filter (λ (r)
            (equal? (hash-ref r 'submitter-id) (current-participant-id)))
          (get/instance 'reviews)))

(define (compute-research-ideas-scores)
  (define (score r)
    (if (hash-ref r 'valid-research-idea?) 1 0))
  (for/fold ([reviewer-scores (hash)])
            ([r (get-reviews-of-participant)])
    ; FIXME: Relies on reviews providing the correct keys. Write more defensive code: either disallow providing the wrong kind of reviews when creating a new review-study, or check here. The former is better.
    (define reviewer (hash-ref r 'reviewer-id))
    (hash-set reviewer-scores
              reviewer
              (+ (score r) (hash-ref reviewer-scores reviewer 0)))))

(define (research-ideas-final-page)
  (define scores (get 'scores))
  (define n (hash-count scores))
  (define score-display
    (if (> n 0)
        (haml
         (:div
          (:h3 "Reviews of your Submission by Reviewer")
          (:ul
           ,@(for/list ([(reviewer score) (in-hash scores)])
               (haml
                (:li (format "Reviewer ~a score: ~a" reviewer score)))))))
        (haml
         (:div
          (:h3 "No Reviews of your Submission available")))))
  (define feedback
    (haml
     (:div
      (:h3 "Detailed Feedback")
      (:div
       ,@(for/list ([r (get-reviews-of-participant)])
           (define valid? (hash-ref r 'valid-research-idea?))
           (define research-idea (hash-ref r 'research-idea))
           (define feedback (hash-ref r 'feedback))
           (haml
            (.feedback
             (:p (:strong "Research Idea: ") research-idea)
             (:ul
              (:li "Feedback: " feedback)
              (:li "Valid: " (if valid? "Yes" "No"))))))))))
  (page
   (haml
    (.container
     (:h1 "End of Review Phase")
     (:p "Thanks for completing your reviews.")
     score-display
     feedback))))

(define (research-ideas-admin)

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
                    (hash-ref r 'research-idea))
              r)))
  (page
   (haml
    (.container
     (:h1 "Admin Interface for a Review Study")

     ; exit admin interface, then re-enter (relies on next step being configured so that we re-enter admin necessarily after exiting), thus passing in the data via `#:require-bindings`

     (:h3 "Research Ideas Submitted")
     (:table
      (:thead
       (:tr
        (:th "Submitter ID")
        (:th "Research Idea")
        (:th "Feedback Given")
        (:th "Review Idea")))
      (:tbody
       ,@(for*/list ([(submitter-id ideas) (in-hash submissions)]
                     [i ideas])
           (haml
            (:tr
             (:td (~a submitter-id))
             (:td (~a i))
             (:td (cond [(hash-ref reviewed (list submitter-id i) #f) => (λ (x) (hash-ref x 'feedback ""))]
                        [else ""]))
             (:td
              (button
               (λ ()
                 (put 'next-submissions (hash submitter-id (list i)))
                 (put 'next-assignments (list submitter-id)))
               "Review this Idea"
               #:to-step-id 'admin-review)))))))

     (:h3 "Research Ideas Reviews")

     (:table
      (:thead
       (:th "Reviewer ID")
       (:th "Reviewed Idea")
       (:th "Valid Idea?")
       (:th "Reviewer Feedback")
       (:th "Comments on Review")
       (:th "Score of Review")
       (:th "Review this Review"))
      (:tbody
       ,@(for/list ([r reviews])
           (match-define (hash-table ('feedback feedback)
                                     ('research-idea research-idea)
                                     ('reviewer-id reviewer-id)
                                     ('submitter-id submitter-id)
                                     ('valid-research-idea? valid?))
             r)
           (haml
            (:tr
             (:td (~a reviewer-id))
             (:td (~a research-idea))
             (:td (~a valid?))
             (:td (~a feedback))
             (:td (~a (hash-ref r 'comments-on-review "")))
             (:td (~a (hash-ref r 'review-score "")))
             (:td
                  (button
                   (λ ()
                     (put 'next-submissions (hash reviewer-id (list r)))
                     (put 'next-assignments (list reviewer-id)))
                   (if (hash-has-key? r 'comments-on-review)
                       "Edit Review of Review"
                       "Review the Review")
                   #:to-step-id 'admin-review-review)))))))))))

(define (admin-review-review)
  (page
   (haml
    (.container
     (:h1 "Admin Review Review")
     (button void "Back to Admin Interface")))))

(define (research-ideas-admin-interface)

  (define (review-in? r rs)
    (match-define (hash-table ('submitter-id s1)
                              ('research-idea i1)
                              ('reviewer-id rv1))
      r)
    (findf (λ (r2)
             (match-define (hash-table ('submitter-id s2)
                                       ('research-idea i2)
                                       ('reviewer-id rv2))
               r2)
             (and (equal? s1 s2)
                  (equal? i1 i2)
                  (equal? rv1 rv2)))
           rs))

  (define (add-admin-reviews new-reviews)
    (with-study-transaction
      (parameterize ([current-study-stack '(*root*)])
        (define reviews (get/instance 'reviews '()))
        (define updated-reviews (append reviews new-reviews))
        (put/instance 'reviews updated-reviews))))

  (define (update-admin-reviews new-reviews)
    (with-study-transaction
      (parameterize ([current-study-stack '(*root*)])
        (define rs
          (filter (λ (r)
                    (not (review-in? r new-reviews)))
                  (get/instance 'reviews)))
        (define updated-reviews
          (append new-reviews rs))
        (put/instance 'reviews updated-reviews))))

  (make-study
   "research-ideas-admin-interface"
   #:provides '()
   #:requires '()
   (list
    (make-step 'admin research-ideas-admin)
    (make-step/study
     'admin-review
     (review-research-ideas)
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

;; FIXME: Design study so that the number of research ideas can be configured by
;; the admin after creating a study instance.
(define submit+review-research-ideas
  (submit+review-study #:submission-study (submit-research-ideas 2)
                       #:review-study (review-research-ideas)
                       #:submission-key 'research-ideas
                       #:compute-scores compute-research-ideas-scores
                       #:final-page research-ideas-final-page
                       #:admin-interface (research-ideas-admin-interface)))
