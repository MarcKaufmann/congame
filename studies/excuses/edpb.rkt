#lang at-exp racket/base

(require congame/components/study
         congame/components/transition-graph
         koyo/haml
         "instructions.rkt")

(provide edpb-pilot)

;;;;;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;;
;;;    Steps that are HTML only      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (welcome)
  (page
   (haml
    @.container{
      @:h1{The Study}

      @:p{Thank you for participating in this study exploring decisions over task allocations.}

      @:p{The research is conducted by Marc Kaufmann and financed by the Central European University. Your participation is voluntary and you may withdraw at any time. Participation in the study is not associated with any forseeable risks or benefits beyond the monetary compensation.}

      @:p{If you have concerns or questions, please contact the principal investigator, Marc Kaufmann, at @"kaufmannm@ceu.edu" or by direct message via Prolific. Report technical problems to @"admin@totalinsightmanagement.com".}

      @button[void]{Continue}})))

;;;;;;;;;;;;;;;; STUDIES

(define ((stub title))
  (page
   (haml
    (.container
     (:h1 title)
     (button void "Next")))))

(define (fail-ending)
  (page
   (haml
    (.container
     (:h1 "You cannot continue")
     (:p "Unforuntately you failed the tutorial, so you cannot continue in this study. Please return the study.")))))

(define (final)
  (page
   (haml
    (.container
     (:h1 "Thank you for participating")))))

(define (tutorial-tasks-stub)
  (page
   (haml
    (.container
     (:h1 "Click below to pass the tutorial tasks")
     (button (lambda ()
               (put 'pass-tutorial? #t))
             "Pass the tutorial")))))

(define day1
  (make-study
   "edpb day 1"
   #:transitions
   (transition-graph
    [sign-up --> work-choices
             --> determine-choice-that-counts
             --> work-day1
             --> schedule-reminder-email
             --> ,(lambda () done)])
   (list
    (make-step 'sign-up (stub "Signup"))
    (make-step 'work-choices (stub "Make work choices"))
    (make-step 'determine-choice-that-counts  (stub "Determine choice that counts"))
    (make-step 'work-day1  (stub "Work Day 1"))
    (make-step 'schedule-reminder-email  (stub "Schedule reminder email")))))


(define day2
  (make-study
   "edpb day 2"
   #:transitions
   (transition-graph
    [work-day2 --> exit-survey --> ,(lambda () done)])
   (list
    (make-step 'work-day2 (stub "Work day 2"))
    (make-step 'exit-survey (stub "Exit survey")))))

  
(define edpb-pilot
  (make-study
   "edpb pilot"
   #:transitions
   (transition-graph
    [welcome --> instructions
             --> hypothetical-time-choice
             ; TODO: Do we really want tutorial tasks? The more uncertainty, the better maybe.
             --> tutorial-tasks
             --> consent
             --> ,(lambda ()
                    (cond [(get 'pass-tutorial?) (goto day1)]
                          [else (goto fail-ending)]))]
    [day1 --> day2 --> send-completion-email --> final]
    [final --> final]
    [fail-ending --> fail-ending])

   (list
    (make-step 'welcome welcome)
    (make-step 'instructions instructions)
    (make-step 'hypothetical-time-choice (stub "Hypothetical Time Choice"))
    (make-step 'tutorial-tasks tutorial-tasks-stub)
    (make-step 'consent (stub "Consent"))
    (make-step/study 'day1 day1)
    (make-step/study 'day2 day2)
    (make-step 'send-completion-email (stub "Send Completion Email"))
    (make-step 'final final)
    (make-step 'fail-ending fail-ending))))
