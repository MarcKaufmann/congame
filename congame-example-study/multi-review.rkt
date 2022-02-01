#lang racket/base

(require congame/components/formular
         congame/components/study
         koyo/haml
         (only-in forms ok err)
         racket/contract
         racket/format
         racket/list
         racket/random
         web-server/http)

;; TODO: support for the admin to set/trigger instance variables. That may best
;; be achieved by having an admin role who can enroll, e.g. by setting the owner
;; as default admin, and then displaying different pages for the admin in
;; different orders. That page sets instance variables that can also be used to
;; configure the study, including reconfiguring it on the fly.

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
      [(= (hash-count submissions) class-size)
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

(define (lobby)
  (if (matchmake)
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
         (:script
          #<<SCRIPT
setTimeout(function() {
  document.location.reload();
}, 1000);
SCRIPT
          ))))))

(define/contract ((final compute-scores display-feedback))
  ; FIXME: compute-score should return a hash of participant-id -> score. This should be checked upon creation of the study with a helpful error message.
  (-> (-> (hash/c integer? number?)) (-> any) any)
  ; FIXME: compute-scores gets called on every refresh, which can be costly.
  ; Recompute scores only if triggered in the admin interface or based on a
  ; timed job.
  (define scores (compute-scores))
  (put 'scores scores)
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

  (page
   (haml
    (.container
     (:h1 "End of Review Phase")
     (:p "Thanks for completing your reviews.")
     score-display
     (display-feedback)
     ))))

(define (update-submissions)
  ;; FIXME: This code will break if we no longer treat files as data, but
  ;; instead deal with them as links. Add a note that `get` and `put` should be
  ;; able to do the right thing when dealing with files? But how do they know?
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
    (put/instance 'reviews updated-reviews))
  (skip))

(define (admin-interface)
  (page
   (haml
    (.container
     (:h1 "Admin Interface for a Review Study")
     (:p "You are in the admin interface")))))

(define (submit+review-study #:submission-study submission-study
                             #:review-study review-study
                             #:submission-key submission-key
                             #:compute-scores [compute-scores (λ () (hash))]
                             #:display-feedback [display-feedback (λ () "")])
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
    (make-step 'check-owner skip
     (λ ()
       (cond [(current-user-owner?)
              'admin-interface]
             [else
              'submit])))
    (make-step 'admin-interface admin-interface)
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
                       (cond [(empty? (get 'assignments)) 'update-reviews]
                             [else 'show-next-review]))
                     #:require-bindings '((assignments assignments)
                                          (submissions submissions))
                     #:provide-bindings '((next-reviews reviews)))
    (make-step 'update-reviews update-reviews)
    (make-step 'final (final compute-scores display-feedback))
    )))

;; REVIEW-PDF

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
  (define next-research-idea (car (get 'current-research-ideas)))
  (define n (get 'n-reviewed-ideas))
  (page
   (haml
    (.container
     (:h1 (format "Review Research Idea ~a (of ~a)" (add1 n) (get 'n-research-ideas)))
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
              (get 'reviews '())))
        (put 'current-research-ideas (cdr (get 'current-research-ideas)))
        (put 'n-reviewed-ideas (add1 (get 'n-reviewed-ideas)))))))))

(define (review-research-ideas)
  (define (next-or-done/transition)
    (cond [(empty? (get 'current-research-ideas))
           done]
          [else
           'review-next-research-idea]))

  (define (initialize-review)
    (define assignments (get 'assignments))
    (define current-assignment (car assignments))
    ; Clear reviews since last review. FIXME: Stateful stuff that is obnoxious
    ; to deal with. One way around this would be to provide reviews only once it
    ; is clear that it is the final call to 'next-assignment-reviews. Any other
    ; strategies?
    (put 'reviews '())
    (put 'current-assignment current-assignment)
    (define submissions (get 'submissions))
    (define current-research-ideas (hash-ref submissions current-assignment))
    (put 'current-research-ideas current-research-ideas)
    (put 'n-reviewed-ideas 0)
    (put 'n-research-ideas (length current-research-ideas))
    (skip))

  (make-study
   "research-ideas-review"
   #:provides '(reviews)
   #:requires '(assignments submissions)
   (list
    (make-step
     'initialize-review
     initialize-review
     next-or-done/transition)
    (make-step
     'review-next-research-idea
     review-next-research-idea
     next-or-done/transition))))

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

(define (research-ideas-display-feedback)
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


;; FIXME: Design study so that the number of research ideas can be configured by
;; the admin after creating a study instance.
(define submit+review-research-ideas
  (submit+review-study #:submission-study (submit-research-ideas 2)
                       #:review-study (review-research-ideas)
                       #:submission-key 'research-ideas
                       #:compute-scores compute-research-ideas-scores
                       #:display-feedback research-ideas-display-feedback))
