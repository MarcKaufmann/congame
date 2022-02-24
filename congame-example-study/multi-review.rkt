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
      (values reviewer (cons reviewer assignments))))
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
         (:p "You have reached the end of the submission phase. Come back once the review phase has started."))))))

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

  (define review-phase?
    (parameterize ([current-study-stack '(*root*)])
      (equal? (get/instance 'phase) 'review)))

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
            (match-define (hash-table ('original-filename submission-filename)
                                      ('submission-id submission-id))
              id&submission)
            (haml
             (:tr
              (:td (~a (->jsexpr submitter-id)))
              (:td (~a submission-id))
              (:td (~a (->jsexpr submission-filename)))
              (:td (cond [(hash-ref reviewed (list submitter-id submission-id) #f)
                          "Yes"]
                         [else "No"]))
              (:td
               (if (not review-phase?)
                   "Submission phase -- cannot review"
                   (button
                    (λ ()
                      (put 'next-submissions (hash submitter-id (list id&submission)))
                      (put 'next-assignments (list submitter-id)))
                    "Review this Submission"
                    #:to-step-id 'admin-review)))))))))))

  (define (submissions-reviews-interface)
    (haml
     (:div
      (:table
       (:thead
        (:th "Reviewer ID")
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
            (match-define (hash-table ('original-filename submission-filename)
                                      ('reviewer-id reviewer-id)
                                      ('submission-id submission-id)
                                      ('submitter-id submitter-id))
              r)
            (haml
             (:tr
              (:td (~a reviewer-id))
              (:td (~a (->jsexpr submission-filename)))
              ,@(for/list ([k keys])
                  (haml
                   (:td (~a (hash-ref r k)))))
              (:td (~a (hash-ref r 'comments-on-review "")))
              (:td (~a (hash-ref r 'review-score "")))
              (:td
               (if (not review-phase?)
                   "Submission Phase -- cannot review"
                   (button
                    (λ ()
                      (put 'next-submissions (hash reviewer-id (list r)))
                      (put 'next-assignments (list reviewer-id)))
                    (if (hash-has-key? r 'comments-on-review)
                        "Edit Review of Review"
                        "Review the Review")
                    #:to-step-id 'admin-review-review)))
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
        (define current-reviews (get/instance 'reviews))
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

; FIXME: Can be refactored modulo formular
(define (submit-single-pdf-submission)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:submission (input-file "Please provide your pdf submission" #:validators (list valid-pdf?)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:submission submission)
        (define submission-upload
          (upload-file! submission))
        (put 'submissions
             (cons (hash 'submission-id (get 'next-submission-id 0)
                         'submission submission-upload)
                   (get 'submissions)))
        (put 'next-submission-id (add1 (get 'next-submission-id 0)))))))))

(define (submit-pdf-submissions [n 2])
  (submit-submissions submit-single-pdf-submission n #:study-name "submit-pdf-submission"))

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
                   (hash-set 'submitted-id (get 'current-assignment))
                   (hash-set 'reviewer-id (current-participant-id))
                   (hash-set 'how-good-is-the-submission how-good-is-the-submission))
               (get 'reviews '()))))))))))

(define (review-pdf)
  (review-submissions review-next-pdf-handler))

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
              (get 'reviews)))))))))

(define (review-reviews-pdf)
  (review-submissions review-next-review-pdfs))

(define (pdf-full-admin-interface)
  (submissions-admin-interface
   pdf-admin-interface-handler
   "PDF study"
   review-pdf
   review-reviews-pdf))

(define (pdf-admin-interface)
  (make-study
   "pdf-admin-interface"
   #:provides '()
   #:requires '()
   (list
    (make-step 'admin (admin-interface-handler) (λ () 'admin)))))

(define submit+review-pdf
  (submit+review-study #:submission-study (submit-pdf-submissions 2)
                       #:review-study (review-pdf)
                       #:submission-key 'submissions
                       #:admin-interface (pdf-full-admin-interface)))

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
              (get 'reviews)))))))))

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

(define (research-ideas-final-page)
  (define scores (get 'scores))
  (put/identity 'scores scores)
  (define participant-reviews (get-reviews-of-participant))
  (define n-reviewers-total (get/instance 'number-of-other-reviewers))
  (define n-reviewers-received
    (~> participant-reviews
        (map (λ (r)
               (hash-ref r 'reviewer-id))
             _)
        remove-duplicates
        length))

  (define score-display
    (haml
     (:div
      (:h4 (format "Total Score: ~a" (hash-ref scores 'total-score)))

      (:h4 "Score by research idea is:")
      (:ul
       ,@(for/list ([(idea score) (in-hash (hash-ref scores 'submission-scores))])
           (haml
            (:li (format "Research Idea: ~a" idea)
             (:ul
              (:li (format "Score: ~a" (~r score #:precision 2)))))))))))


  (define feedback
    (haml
     (:div
      (:h3 (format "Detailed Reviews (~a received, ~a pending)"
                   n-reviewers-received
                   (- n-reviewers-total n-reviewers-received)))
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
     feedback))))

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
