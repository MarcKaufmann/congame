#lang at-exp racket/base

(require racket/format
         koyo/haml
         congame/components/study
         congame/components/transition-graph
         congame/components/formular
         (submod congame/components/formular tools)
         (submod congame/components/study accessors)
         "templates.rkt"
         "abstract-categorization.rkt")

(provide edpb-pilot)

;;;;;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;;
;;;    Steps that are HTML only      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (welcome)
  (page
   (haml
    @.container{
      @:h1{The Study}

      @:p{Thank you for participating in this study examining how people make decisions for work over time.}

      @:p{This study is conducted by Flora Drucker and Marc Kaufmann and financed by Central European University. Your participation is voluntary and if you accept to participate, you may withdraw at any time. However, please note that you will receive some bonuses if you complete specific parts of the study. This is described in more detail on the Instructions page.}

      @:p{Participation in this study is not associated with any foreseeable risk or benefit. Your answers will be collected confidentially and anonymously (the researchers will not be able to link decisions and participants' identity beyond the Prolific ID provided). At the data analysis stage your Prolific ID will be changed to a random identifying number, and the Prolific IDs will be deleted. In case the results of the study are published, there can be no references to your identity. Data anonymity is guaranteed.}

      @:p{This study received a research ethics approval from the Ethical Research Committee of Central European University.}

      @:p{If you have any questions or concerns regarding this study, please contact us at @"admin@totalinsightmanagement.com" or @"lucafloradrucker.research@gmail.com".}

      @button[void]{Continue}})))

(define (hypothetical-time-choice)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:patience
         (input-number
          "How willing are you to give up something that is beneficial for you today in order to benefit more from that in the future? (0 means not willing at all, 10 means very willing)"
          #:min 0 #:max 10))
        submit-button)))))))

(define ((stub title))
  (page
   (haml
    (.container
     (:h1 title)
     (button void "Next")))))

(define (no-consent-ending)
  (page
   (haml
    (.container
     (:h1 "You decided not to participate in the study")
     @:p{Thank you for participating this far. Please provide the code NOCONSENT on prolific to receive your default participation fee.}))))

(define (final)
  (page
   (haml
    (.container
     (:h1 "Thank you for participating")))))


(define (consent)
  (page
   (haml
    (.container
     (:h1 "Consent")

     @:p{You have now completed the introduction to the study: you have read the instructions, the task description, and the payment conditions. Based on this, you can now decide whether to participate in the study and continue, or whether to stop after the introduction ends.}

     (formular
      (haml
       (:div
        (#:consent-given?
         (radios "" '(("agree"    . "I agree to participate in the study")
                      ("disagree" . "I do not agree to participate in the study and stop here"))))
        submit-button))
      (lambda (#:consent-given? consent?)
        (put 'consent-given? (string=? consent? "agree"))))))))

;; TODO: participants can take the comprehension test 3 times (fail twice), then they are out.
(define (comprehension-test)
  (define attempt
    (cond [(get #:round "" 'attempt #f) => values]
          [else (begin0 1
                  (put #:round "" 'attempt 1))]))
  (set-current-round-name! (~a "attempt " attempt))
  (page
   (haml
    (.container
     (:h1 (format "Comprehension Test (~a attempt)" attempt))

     ;; FIXME: add actual comprehension questions
     ;; FIXME: provide information (instructions etc) to answer the questions at the bottom of the comprehension form.
     (formular
      (haml
       (:div
        (#:comprehension1
         (radios
          "What is 1 + 1? FIXME"
          '(("0" . "0")
            ("1" . "1")
            ("2" . "2")
            ("?" . "I don't know"))))
        submit-button))
        (lambda (#:comprehension1 comprehension1)
          (define score
            (apply
             +
             (map
              (λ (b) (if b 1 0))
              (list
               (string=? comprehension1 "2")))))
          (put
           #:round "" 'attempt (add1 (get #:round "" 'attempt)))
          (put 'comprehension-test-score score)))))))

(define max-attempts 3)

(define (repeat-comprehension-test)
  (define next-attempt
    (get #:round "" 'attempt))
  (page
   (haml
    (.container
     (:h1 (format
           "You failed the comprehension times for the ~a time"
           (case (sub1 next-attempt)
             [(1) "first"]
             [(2) "second"])))

     @:p{Remember: You can fail the test at most @(~a max-attempts) times, otherwise you drop out of the study without payments. Try again.}

     (button void "Go to comprehension test")))))

(define (fail-comprehension-test)
  (page
   (haml
    (.container
     @:h1{You failed the comprehension test too many times}

     @:p{Unfortunately, you failed the comprehension test too many times. Please return the study.}))))

;;;;;;;;;;;;;;;; STUDIES

(define (comprehension-test-success?)
  (> (get 'comprehension-test-score) 0))

(define (tutorial)
  (define (initialize)
    (put/instance 'n 2)
    (put 'n 2)
    (put/instance 'tutorial-example
                  (random-abstract-matching "Equality" "Other"))
    (skip))

  (make-study
   "edpb-tutorial"
   #:transitions
   (transition-graph
    [initialize --> welcome
                --> hypothetical-time-choice
                --> instructions
                --> task-description
                ; TODO: Do we really want tutorial tasks? The more uncertainty, the better maybe.
                --> tutorial-tasks
                --> comprehension-test
                --> ,(lambda ()
                       (cond [(comprehension-test-success?)
                              (set-current-round-name! "")
                              done]

                             [(<= (get #:round "" 'attempt) max-attempts)
                              (goto repeat-comprehension-test)]

                             [else
                              (set-current-round-name! "")
                              (goto fail-comprehension-test)]))]

    [repeat-comprehension-test --> comprehension-test]
    [fail-comprehension-test --> fail-comprehension-test])

   (list
    (make-step 'initialize initialize)
    (make-step 'welcome welcome)
    (make-step 'instructions instructions)
    (make-step 'hypothetical-time-choice hypothetical-time-choice)
    ; TODO: Is this type of thunk a good way to hook up functional pages with data? If so, provide syntactic sugar for it.
    (make-step
     'task-description
     (lambda ()
       (task-description
        (get/instance 'n)
        (get/instance 'tutorial-example))))
    (make-step 'comprehension-test comprehension-test)
    (make-step/study
     'tutorial-tasks
     (abstract-tasks)
     #:require-bindings '([n             n]
                          [category      (const "Gender")]
                          [non-category  (const "Other")]))
    (make-step 'repeat-comprehension-test repeat-comprehension-test)
    (make-step 'fail-comprehension-test fail-comprehension-test))))


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

(define (assigning-roles)
  (cond [(current-participant-owner?)
         (put/top 'role 'admin)
         (skip)]
        [else
         (put/top 'role 'participant)
         (skip)]))

(define (study-open?)
  (equal? (get/instance/top 'phase #f) 'open))

(define (waiting-page)
  (cond [(study-open?)
         (skip)]

        [else
         (page
          (haml
           (.container
            (:h1 "The study is not yet open")

            (:p "The study is not yet open for participants. Please come back later.")
            (:p "If you believe this is in error, please send an email to the study admin."))))]))

(define (admin)
  (page
   (haml
    (.container
     (:h1 "Admin")

     (unless (study-open?)
       (button void "Setup study" #:to-step-id 'abstracts-admin))))))

(define (switch-phase-to p #:check-current-phase [cp #f])
  (define old-phase (get/instance/top 'phase #f))
  (cond [(or (not cp) (and cp (equal? cp old-phase)))
         (put/instance/top 'phase p)]

        [else
         (error 'switch-phase-to "failed because the current phase is ~a, but needs to be ~a to switch phase" old-phase cp)]))


(define edpb-pilot
  (make-study
   "edpb pilot"
   #:transitions
   (transition-graph
    [assigning-roles --> ,(lambda ()
                            (let ([role (get 'role)])
                              (cond [(equal? role 'admin)       (goto admin)]
                                    [(equal? role 'participant) (goto waiting-page)]
                                    [else                       (goto error-page)])))]
    [error-page --> error-page]
    [admin --> admin]
    [abstracts-admin --> ,(lambda ()
                            (switch-phase-to 'open #:check-current-phase #f)
                            (goto admin))]
    [waiting-page --> tutorial
                  --> consent
                  --> ,(lambda ()
                         (cond [(get 'consent-given?) (goto day1)]
                               [else (goto no-consent-ending)]))]

    [day1 --> day2 --> send-completion-email --> final]
    [final --> final]
    [no-consent-ending --> no-consent-ending])

   (list
    (make-step 'assigning-roles assigning-roles)
    (make-step/study 'tutorial (tutorial))
    (make-step 'admin admin)
    (make-step/study 'abstracts-admin abstracts-admin)
    (make-step 'waiting-page waiting-page)
    (make-step 'error-page (stub "Error page"))
    (make-step 'consent consent)
    (make-step/study 'day1 day1)
    (make-step/study 'day2 day2)
    (make-step 'send-completion-email (stub "Send Completion Email"))
    (make-step 'final final)
    (make-step 'no-consent-ending no-consent-ending))))
