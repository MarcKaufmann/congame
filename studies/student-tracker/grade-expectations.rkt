#lang racket/base

(require koyo/haml
         gregor
         congame/components/formular
         (submod congame/components/formular tools)
         congame/components/study
         congame/components/transition-graph
         "../stdlib.rkt")

(provide grade-expectations)

(define survey-duration 3) ; How long the surveys remain open.

; NOTE: We'll leave the surveys open for one week
(define survey-dates
  (list
   ; FIXME: Add actual dates
   (today) ; #t #f "Problem Set/Midterm/Final"
   (+days (today) 1)))

(define (schedule-reminder-email d)
  void)


(define (survey-is-open? d)
  (define t (today))
  (and (date<=? d t)
       (date<=? t (+days d survey-duration))))

(define (survey-is-over? d)
  (date<=? (+days d survey-duration) (today)))

(define (put/date k d)
  (put k (date->iso8601 d)))

(define (get/date k)
  (iso8601->date (get k)))

(define (waiting-page)
  (define t (today))
  (define next-survey-date
    (get/date 'next-survey-date))
  (cond [(survey-is-open? next-survey-date)
         (skip)]

        [(survey-is-over? next-survey-date)
         (skip 'schedule-next-survey-or-done)]

        [else
         (page
          (haml
           (.container
            (:h1 "The next survey is not yet open")

            (:p (format "The next survey only opens on ~a. Please come back then." (~t next-survey-date "EEEE, MMMM d"))))))]))

(define (input-percent/grade g)
  (input-number (format "What is the percent chance that you will get a final grade of ~a or more in this course? (0-100)" g)
                #:min 0 #:max 100))
(define (date-key)
  (string->symbol (string-append "date-" (get 'next-survey-date))))

(define (survey)
  (page
   (haml
    (.container
     (:h1 "Survey of Grade Expectations")

     (formular
      (haml
       (:div
        (:div
         (#:grade
          (radios
           "What grade do you expect in this course?"
           '(("A" . "A")
             ("A-" . "A-")
             ("B+" . "B+")
             ("B"  . "B")
             ("B-" . "B-")
             ("C+" . "C+")
             ("F"  . "F")))))
        (:div
         (#:grade-above-A-minus
          (input-percent/grade "A-"))
         (#:grade-above-B-minus
          (input-percent/grade "B"))
         (#:grade-above-C-plus
          (input-percent/grade "C+")))
        (:div
         (#:final-score
          (input-number "What score from 0 to 100 do you expect on the final exam (not on the course)?" #:min 0 #:max 100))
         ; FIXME: Do this in a follow-up page?
         (#:final-score-certainty
          (input-number "How likely it is that your score on the final will be 10 or more points below the score you just reported?"
                        #:min 0 #:max 100)))
        submit-button))
      (make-put-form/hash (date-key)))))))

(define (report-grade)
  (page
   (haml
    (.container
     ; FIXME: Make flexible heading to allow for midterm
     (:h1 "Report your problem set grade")
     (formular
      (haml
       (:div
       (#:problem-set-grade
        (input-number "What grade did you get on the most recent problem set?" #:min 0 #:max 100)))
       submit-button)
      (make-put-form/hash (date-key)))))))

(define (schedule-next-survey-or-done)
  (define survey-dates (get 'remaining-survey-dates))
  (cond [(null? survey-dates)
         (skip 'thank-you)]

        [else
         ; NOTE: We cannot put a `skip` inside a transaction. Ask Bogdan why.
         (with-study-transaction
           (define next-survey-date
             (iso8601->date (car survey-dates)))
           (schedule-reminder-email next-survey-date)
           (put/date 'next-survey-date next-survey-date)
           (put 'remaining-survey-dates
                (map date->iso8601 (cdr survey-dates))))
         (skip 'waiting-page)]))

(define (thank-you)
  (page
   (haml
    (.container
     (:h1 "Thank you")
     (:p "Thank you for participating in the study, you have completed it.")))))

(define grade-expectations
  (make-study
   "panel study for grade expectations"
   #:transitions
   (transition-graph
    [assigning-participant&owner --> ,(lambda ()
                                        (let ([role (get 'role)])
                                          (case role
                                            [(admin) (goto admin)]
                                            [(participant)
                                             (put/date 'next-survey-date (car survey-dates))
                                             (put 'remaining-survey-dates (map date->iso8601 (cdr survey-dates)))
                                             (goto waiting-page)]
                                            [else 'error-page])))]
    [admin --> admin]

    [waiting-page --> report-grade
                  --> expectations-survey
                  ; this will skip to waiting-page, unless there are no more surveys
                  --> schedule-next-survey-or-done
                  --> thank-you]

    [error-page --> error-page]

    [thank-you --> thank-you])

   (list
    (make-step 'assigning-participant&owner assigning-participant&owner)
    (make-step 'admin (make-stub "Admin"))
    (make-step 'waiting-page waiting-page)
    (make-step 'report-grade report-grade)
    (make-step 'expectations-survey survey)
    (make-step 'schedule-next-survey-or-done schedule-next-survey-or-done)
    (make-step 'error-page (make-stub "Error Page"))
    (make-step 'thank-you thank-you))))
