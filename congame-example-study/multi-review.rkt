#lang racket/base

(require congame/components/formular
         congame/components/study
         koyo/haml
         (only-in forms ok err)
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

(define (matchmake)
  (with-study-transaction
    (define submissions (get/instance 'submissions (hash)))
    (cond
      [(= (hash-count submissions) class-size)
       (define assignments (get/instance 'assignments #f))
       (cond
         [assignments #t]
         [else
          (define participant-ids (hash-keys submissions))
          (put/instance 'assignments (assign-reviewers participant-ids))
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

(define (final)
  (page
   (haml
    (.container
     (:h1 "End of Review Phase")
     (:p "You're done. Thanks for reviewing!")))))

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

(define (submit+review-study #:submission-study submission-study
                             #:review-study review-study
                             #:submission-key submission-key)
  (make-study
   "review-study"
   #:requires '()
   #:provides '()
   (list
    (make-step/study 'submit submission-study
                     #:provide-bindings `([submission ,submission-key]))
    (make-step 'update-submissions update-submissions)
    (make-step 'lobby lobby)
    (make-step/study 'review-1 review-study
                     #:require-bindings '((assignments assignments)
                                                                 (submissions submissions))
                     #:provide-bindings '((assignments assignments)))
    ; FIXME: Ideally the submissions are just stored on the instance. However,
    ; they are stored at the '(*root*) stack, not '(*root* review-study) where
    ; they are accessed. Currently we can only pass around user-variables, not
    ; instance-variables, which means I am copying all the submissions over and
    ; over. While we could only copy the needed submissions, the lack of such a
    ; feature is problematic, and having to pass the whole variable (rather than
    ; a reference) around is too. That would require having non-mutable data,
    ; unless otherwise specified, with even more complications. Currently this
    ; is likely to be an issue only for files or for lots of copying.
    (make-step/study 'review-2 review-study
                     #:require-bindings '((assignments assignments)
                                                                 (submissions submissions))
                     #:provide-bindings '((assignments assignments)))
    (make-step 'final final)
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
                       'submission submission-file
                       'how-good-is-the-submission how-good-is-the-submission))
         (put 'assignments (cdr assignments)))))))))

(define review-pdf
  (make-study
   "review-pdf"
   #:provides '(assignments)
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

(define ((review-research-ideas-handler n))
  (define assignments (get 'assignments))
  (define submissions (get 'submissions))
  (define research-ideas (hash-ref submissions (car assignments)))
  (define research-ideas-rubric-url "https://trichotomy.xyz")
  (page
   (haml
    (.container
     (:h1 "Review these Research Ideas")
     (.submission
      (:h3 "Submitted Research Ideas")
      (:ol
       ; TODO: Put in documentation that these things need to be spliced in
       ,@(for/list ([idea research-ideas])
           (haml
            (:li idea)))))
     (:div
      (:h3 "Rubric for Research Ideas")
      (:p "For further details on this rubric, see "(:a ((:href research-ideas-rubric-url)) "Explanations on Research Ideas") ".")
      (formular
       (haml
        (:div
         (#:how-many-ideas (input-number "How many ideas have been provided (ignore whether they are valid or not in your opinion)?"
                                         #:min 0 #:max n))
         (#:how-many-valid-ideas (input-number "How many valid research have been provided?"
                                               #:min 0
                                               #:max n))
         (#:provide-feedback (input-textarea "If you think there are invalid ideas, please pick one of them, state why you believe it is not valid, and provide constructive suggestion for turning it into a valid research idea."))
         (:button.button.next-button ((:type "submit")) "Submit")))
       (lambda (#:how-many-ideas how-many-ideas
                #:how-many-valid-ideas how-many-valid-ideas
                #:provide-feedback provide-feedback)
         (put 'review (hash
                       'submitter-id (car assignments)
                       'research-ideas research-ideas
                       'how-many-ideas how-many-ideas
                       'how-many-valid-ideas how-many-valid-ideas
                       'provide-feedback provide-feedback))
         (put 'assignments (cdr assignments)))))))))

(define (review-research-ideas n)
  (make-study
   "research-ideas-review"
   #:provides '(assignments)
   #:requires '(assignments submissions)
   (list
    (make-step
     'review-research-ideas
     (review-research-ideas-handler n)))))

;; FIXME: Design study so that the number of research ideas can be configure by
;; the admin after creating a study instance.
(define submit+review-research-ideas
  (submit+review-study #:submission-study (submit-research-ideas 2)
                       #:review-study (review-research-ideas 2)
                       #:submission-key 'research-ideas))
