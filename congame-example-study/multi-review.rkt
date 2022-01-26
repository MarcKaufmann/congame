#lang racket/base

(require congame/components/formular
         congame/components/study
         koyo/haml
         (only-in forms ok err)
         racket/list
         racket/random
         web-server/http)

;; TODO: support for the admin to set/trigger instance variables

;; design:
;; * the entire class is part of the same group
;; * the size of the group is predeterimened (eg. 10 people total)
;; * first, everybody has to submit a submission before anybody can make progress
;; * everybody is assigned to review 2 other random people from the group

(provide
 review-pdf
 review-research-ideas
 submit-two-research-ideas)

(define class-size 3)

;; FIXME: the worst-case complexity here is really bad.  There must be
;; a way to do this without drawing randomly and retrying on failure.
(define (assign-reviewers pids [n 1])
  (when (<= (length pids) n)
    (raise-arguments-error 'assign-reviewers "pids must be > n + 1" "pids" pids "n" n))
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
        (with-study-transaction
          (define submission-file (put/instance-file submission))
          (define submissions (get/instance 'submissions (hash)))
          (define updated-submissions (hash-set submissions (current-participant-id) submission-file))
          (put/instance 'submissions updated-submissions))))))))

(define provide-pdf-submission/study
  (make-study
   "provide-pdf-submission"
   #:requires '()
   #:provides '()
   (list
    (make-step 'provide-pdf-submission provide-pdf-submission))))

(define (lobby)
  (if (matchmake)
      (page
       (button
        (lambda ()
          (define all-assignments (get/instance 'assignments))
          (put 'assignments (hash-ref all-assignments (current-participant-id))))
        "Continue."))
      (page
       (haml
        (:div
         (:p "Please wait for more participants to join.")
         (:script
          #<<SCRIPT
setTimeout(function() {
  document.location.reload();
}, 1000);
SCRIPT
          ))))))

(define (review)
  (define submissions (get/instance 'submissions))
  (define assignments (get 'assignments))
  (define submission-file (get/instance-file (hash-ref submissions (car assignments))))
  (page
   (haml
    (:div
     (:p "Please review this submission.")
     (:p
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
     (button
      (lambda ()
        (put 'assignments (cdr assignments)))
      "Continue")))))

(define (final)
  (page
   (haml
    (:p "You're done. Thanks!"))))

(define (update-submissions)
  ; FIXME: This code does not work for files, it assumes direct data.
  (with-study-transaction
    (define submission (get 'submission))
    (define submissions (get/instance 'submissions (hash)))
    (define updated-submissions (hash-set submissions (current-participant-id) submission))
    (put/instance 'submissions updated-submissions))
  (skip))

(define (review-study submission-study)
  (make-study
   "review-study"
   #:requires '()
   #:provides '()
   (list
    (make-step/study
     'submit
     submission-study
     #:provide-bindings '([submission research-ideas]))
    (make-step 'update-submissions update-submissions)
    (make-step 'lobby lobby)
    (make-step 'review-1 review)
    (make-step 'review-2 review)
    (make-step 'final final))))

(define review-pdf
  (review-study provide-pdf-submission/study))

;;; Assignments to use with multi-review

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
           done]))

  (make-study
   "research-ideas-study"
   #:requires '()
   #:provides '(research-ideas)
   (list
    (make-step 'initialize initialize next-or-done/transition)
    (make-step 'submit-research-idea submit-research-idea next-or-done/transition))))

(define submit-two-research-ideas

  (make-study
   "submit-two-research-ideas"
   #:requires '()
   #:provides '()
   (list
    (make-step/study
     'sutmit-two-research-ideas
     (submit-research-ideas))
    (make-step 'done final))))

(define review-research-ideas
  (review-study (submit-research-ideas 2)))
