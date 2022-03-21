#lang racket/base

(require congame/components/formular
         congame/components/study
         koyo/haml
         racket/format
         threading
         web-server/http
         "tools.rkt")

(provide
 instructor-review/study)

(define (start)
  (define phase (get/instance 'phase 'admin-setup))
  (cond [(equal? phase 'admin-setup)
         (page
          (haml
           (.container
            (:h1 "Study is not yet live")
            (:p "Please come back later."))))]
        [(equal? phase 'submission)
         (page
          (haml
           (.container
            (:h1 "Study is open")
            (button
             void
             "Go to Instructions and Submission"))))]
        [else
         (error "No such phase")]))

(define (submit)
  (define instructions-upload
    (get/instance 'instructions-file))
  (page
   (haml
    (.container
     (:h3 "Instructions")
     (file-download/link instructions-upload "Instructions")

     (:h3 "Submit your pdf")
     (formular
      (haml
       (:div
        (#:submission (input-file "Please provide your pdf submission" #:validators (list valid-pdf?)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:submission submission)
        (define upload
          (upload-file! submission))
        (put 'submission upload)
        (put/instance
         'submissions
         (hash-set (get/instance 'submissions (hash))
                   (current-participant-id)
                   upload))))))))

(define (final-page)
  (define pid (current-participant-id))
  (define score
    (hash-ref (get/instance 'scores (hash)) pid #f))
  (define graded-pdf
    (hash-ref (get/instance 'graded-pdfs (hash)) pid #f))
  (define f (get 'submission))
  (page
   (haml
    (.container
     (:h1 "Submission Completed")
     (:p "We have have received " (file-download/link f "your submission."))
     (:h3 "Score and Grading")
     (cond [(and score graded-pdf)
            (haml
             (:div
              (:ul
               (:li "Score: " (~a score))
               (:li (file-download/link graded-pdf "Comments/Marking")))))]
           [else
            (haml
             (:div
              (:p "Once your submission is graded, you will see the information here.")))])
     (if (equal? (get/instance 'phase) 'submission)
         (button
          void
          "Replace your submission"
          #:to-step-id 'submit)
         (haml
          (:div "")))))))

(define (admin-setup)
  (page
   (haml
    (.container
     (:h1 "Admin")
     (formular
      (haml
       (:div
        (#:instructions-file (input-file "Provide the pdf with instructions for the students"))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:instructions-file instructions-file)
        (define uploaded-instructions
          (upload-file! instructions-file))
        (put 'instructions-file uploaded-instructions)
        (put/instance 'instructions-file uploaded-instructions)
        (put/instance 'phase 'submission)))))))

(define (admin)
  (define scores (get/instance 'scores (hash)))
  (define submissions (get/instance 'submissions (hash)))
  (page
   (haml
    (.container
     (:h1 "Admin")

     (:h3 "Submissions")

     (:table
      (:thead
       (:tr
        (:th "Participant")
        (:th "Submission")
        (:th "Score")
        (:th "Marked PDF")
        (:th "Provide Score")
        (:th "Provide comments/marked PDF")))
      ,@(for/list ([(pid s) (in-hash submissions)])
          (define graded-pdf
            (hash-ref (get/instance 'graded-pdfs (hash)) pid #f))
          (haml
           (:tr
            (:td (~a pid))
            (:td (file-download/link s "Download File"))
            (:td (~a (hash-ref scores pid "No score yet")))
            (:td (if graded-pdf
                     (file-download/link graded-pdf "Graded PDF")
                     "Not yet graded"))
            (:td (button
                  (λ ()
                    (put 'admin-score-participant pid))
                  "Submit Score"
                  #:to-step-id 'admin-submit-score))
            (:td (button
                  (λ ()
                    (put 'admin-provide-pdf-participant pid))
                  "Submit comments/pdf"
                  #:to-step-id 'admin-submit-graded-pdf))))))

     (:h3 "Actions")

     (button void "Add/Change Grader Email" #:to-step-id 'admin-grader-email)
     (if (equal? 'submission (get/instance 'phase))
         (button
          (λ ()
            (put/instance 'phase 'review))
          "Close Submissions")
         (haml (:div "")))))))

(define (admin-submit-score)
  (define scores (get/instance 'scores (hash)))
  (define pid (get 'admin-score-participant))
  (define file-to-grade
    (hash-ref (get/instance 'submissions) pid))
  (page
   (haml
    (.container
     (:h1 "Submit Score")
     (:p (:strong "Note: ") "You are scoring " (file-download/link file-to-grade "this submission") ".")
     (formular
      (haml
       (:div
        (#:score (input-number "What score do you give this submission?"
                               #:min 0 #:max 20))
        (:button.button.next-button ((:type "submit")) "Submit")))
      (lambda (#:score score)
        (put/instance
         'scores
         (hash-set scores pid score))))))))

(define (admin-submit-graded-pdf)
  (define graded-pdfs (get/instance 'graded-pdfs (hash)))
  (define pid (get 'admin-provide-pdf-participant))
  (define file-to-grade
    (hash-ref (get/instance 'submissions) pid))
  (page
   (haml
    (.container
     (:h1 "Provide Graded PDF")
     (:p (:strong "Note: ") "You are providing the graded pdf for " (file-download/link file-to-grade "this submission") ".")
     (formular
      (haml
       (:div
        (#:graded-pdf (input-file "Provide graded PDF" #:validators (list valid-pdf?)))
        (:button.button.next-button ((:type "submit")) "Submit")))
      (lambda (#:graded-pdf graded-pdf)
        (define uploaded-pdf
          (upload-file! graded-pdf))
        (put/instance
         'graded-pdfs
         (hash-set graded-pdfs pid uploaded-pdf))))))))

(define (admin-grader-email)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:grader-email (input-text "The grader's email"))
        (:button.button.next-button ((:type "submit")) "Submit")))
      (lambda (#:grader-email grader-email)
        (put/instance 'grader-email grader-email)))))))

(define instructor-review/study
  (make-study
   "instructor-review"
   #:provides '()
   #:requires '()
   (list
    (make-step
     'check-owner
     skip
     (λ ()
       (define grader-email (get/instance 'grader-email #f))
       (cond [(current-participant-owner?)
              (put/instance 'phase 'admin-setup)
              'admin-setup]
             [(and grader-email
                   (equal? (participant-email (current-participant-id))
                           grader-email))
              'admin]
             [else
              'start])))
    (make-step 'admin-setup admin-setup (λ () 'admin))
    (make-step 'admin-submit-graded-pdf admin-submit-graded-pdf (λ () 'admin))
    (make-step 'admin-submit-score admin-submit-score (λ () 'admin))
    (make-step 'admin-grader-email admin-grader-email (λ () 'admin))
    (make-step 'admin admin (λ () 'admin))
    (make-step 'start start)
    (make-step 'submit submit)
    (make-step 'final final-page))))
