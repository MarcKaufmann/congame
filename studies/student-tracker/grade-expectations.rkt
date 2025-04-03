#lang racket/base

(require racket/contract
         koyo/haml
         gregor
         congame/components/formular
         (submod congame/components/formular tools)
         congame/components/study
         congame/components/transition-graph
         congame-web/components/identity
         "../stdlib.rkt")

; FIXME: Add date of survey to the title, just to make sure things are as expected
(provide grade-expectations)

(define survey-duration 2) ; How long the surveys remain open.

(define (sd id day survey? report? report-type)
  (hash 'id id
        'day day
        'survey? survey?
        'report? report?
        'report-type report-type))

(define ((sd-ref k) s)
  (hash-ref s k))

(define sd-id (sd-ref 'id))
(define sd-day (sd-ref 'day))
(define sd-survey? (sd-ref 'survey?))
(define sd-report? (sd-ref 'report?))
(define sd-report-type (sd-ref 'report-type))

(define (sd->iso8601 s)
  (hash-update s 'day (lambda (v) (date->iso8601 v))))

(define (iso8601->sd s)
  (hash-update s 'day (lambda (v) (iso8601->date v))))

(define surveys
  (let ([t (today)])
    (list
     (sd 1 (date 2023 09 20) #t #f "")
     (sd 2 (date 2023 10  4) #t #f "")
     (sd 3 (date 2023 10 18) #t #t "Problem Set")
     (sd 4 (date 2023 10 24) #t #f "")
     (sd 5 (date 2023 11  1) #t #t "Problem Set")
     (sd 6 (date 2023 11 15) #t #t "Midterm")
     (sd 7 (date 2023 11 29) #t #t "Problem Set")
     (sd 8 (date 2023 12  4) #t #f "")
     (sd 9 (date 2023 12  7) #t #f ""))))

(define (survey-is-open? d)
  (define t (today))
  (and (date<=? d t)
       (date<=? t (+days d survey-duration))))

(define (survey-is-over? d)
  (date<=? (+days d survey-duration) (today)))

(define/contract (put/date k d)
  (-> symbol? date? any/c)
  (put k (date->iso8601 d)))

(define (get/date k)
  (iso8601->date (get k)))

(define (waiting-page)
  (define t (today))
  (define next-survey
    (sd-day (iso8601->sd (get 'next-survey))))
  (cond [(survey-is-open? next-survey)
         (put-current-round-name (date->iso8601 next-survey))
         (skip)]

        [(survey-is-over? next-survey)
         (skip 'schedule-next-survey-or-done)]

        [else
         (haml
          (.container
           (:h1 "The next survey is not yet open")

           (:p (format "The next survey only opens on ~a. Please come back then." (~t next-survey "EEEE, MMMM d")))))]))

(define (input-percent/grade g)
  (input-number (format "What is the percent chance that you will get a final grade of ~a or more in this course? (0-100)" g)
                #:min 0 #:max 100))

(define (date-key)
  (string->symbol (string-append "date-" (sd-day (get 'next-survey)))))

(define (put/round k v)
  (put #:round (get-current-round-stack)
       k v))

(define (survey)
  (define next-survey
    (iso8601->sd (get 'next-survey)))

  (unless (sd-survey? next-survey)
    (skip))

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
     (make-put-form/hash (string->symbol (format "survey-~a" (sd-id next-survey))))))))

(define (report-grade)
  (define next-sd
    (get 'next-survey))
  (unless (sd-report? next-sd)
    (skip))

  (define report-type
    (sd-report-type next-sd))
  (haml
   (.container
    ; FIXME: Make flexible heading to allow for midterm
    (:h1 (format "Report your grade on ~a" report-type))
    (formular
     (haml
      (:div
       (#:problem-set-grade
        (input-number (format "What grade did you get on ~a? (0-100)" report-type) #:min 0 #:max 100))
       submit-button))
     (make-put-form/hash (string->symbol (format "report-~a" (sd-id next-sd))))))))

(define (schedule-next-survey-or-done)
  (define surveys
    (map iso8601->sd (get 'remaining-surveys)))
  (cond [(null? surveys)
         (skip 'thank-you)]

        [else
         ; NOTE: We cannot put a `skip` inside a transaction. Ask Bogdan why.
         (with-study-transaction
           (eprintf "getting next survey date~n")
           (define next-survey
             (car surveys))
           (eprintf "putting next survey date~n")
           (put 'next-survey (sd->iso8601 next-survey))
           (eprintf "putting remaining survey dates~n")
           (eprintf "remaining survey dates: ~a~n" (cdr surveys))
           (put 'remaining-surveys
                (map sd->iso8601 (cdr surveys))))
         (skip 'waiting-page)]))

(define (thank-you)
  (haml
   (.container
    (:h1 "Thank you")
    (:p "Thank you for participating in the study, you have completed it."))))

(define (keep-score)
  (define score
    (get 'score 0))
  (define new-score
    (add1 score))
  (put 'score new-score)
  (put/identity 'score new-score)
  (skip))

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
                                             (define all-surveys
                                               (map sd->iso8601 surveys))
                                             (put 'next-survey (car all-surveys))
                                             (put 'surveys all-surveys)
                                             (put 'remaining-surveys (cdr all-surveys))
                                             (goto waiting-page)]
                                            [else 'error-page])))]
    [admin --> admin]

    [waiting-page --> report-grade
                  --> expectations-survey
                  --> keep-score
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
    (make-step 'keep-score keep-score)
    (make-step 'schedule-next-survey-or-done schedule-next-survey-or-done)
    (make-step 'error-page (make-stub "Error Page"))
    (make-step 'thank-you thank-you))))
