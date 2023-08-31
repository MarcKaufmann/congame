#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/match
         racket/serialize
         (only-in xml xexpr/c)
         koyo/haml
         koyo/url
         congame/components/study
         congame/components/transition-graph
         congame/components/formular
         (submod congame/components/formular tools)
         (submod congame/components/study accessors)
         "templates.rkt"
         "abstract-categorization.rkt")

(provide
 edpb-intro
 edpb-main)

;; TODO
;; - Add intro-completion-code to the instance level of edpb-intro and edpb-main (where people receive it if they continue with the study)
;; - Add information to the study and to the consent form that they need to signup with their prolific email to do the main study.
;; - Change the study so that people have to sign up right away, after providing just their prolific ID and their answer to the patience question (to see selection). That way, we don't need to merge any of the data across studies and I do not have to wait for Bogdan to implement anything. We should still implement talking between instances.
;; - create a prolific only signup page, i.e. people have to type in a prolific email or ID. This avoids them signing up with personal emails.


;;;;;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;;
;;;    Steps that are HTML only      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ((stub title))
  (page
   (haml
    (.container
     (:h1 title)
     (button void "Next")))))

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
    (cond [(get 'attempt #f) => values]
          [else (begin0 1
                  (put 'attempt 1))]))
  (put-current-round-name (~a "attempt " attempt))
  (page
   (haml
    (.container
     (:h1 (format "Comprehension Test (~a attempt)" attempt))

     ;; FIXME: provide information (instructions etc) to answer the questions at the bottom of the comprehension form.
     (formular
      (haml
       (:div
        (:div
         (#:when-paid
          (radios
           "When will you receive the payments?"
           '(("1" . "Immediately after the first session.")
             ("2" . "Immediately after the second session.")
             ("3" . "Within three days after the second session.")))))
        (:div
         (#:session1-payment
          (radios
           "Will you receive any payment if you only complete the first session?"
           '(("1" . "No, only if I complete both sessions.")
             ("2" . "Yes, I receive a completion bonus for the first session within three days after the corresponding second session.")
             ("3" . "Yes, I receive a completion bonus for the first session immediately after the first session.")
             ("4" . "Yes, I receive a completion bonus for the first session and extra payment if I choose to work in the first session within three days after the corresponding second session.")))))
        (:div
         (#:how-many-abstracts
          (radios
           "If the decision to implement is '25 abstracts as animal rights vs. other in the second session', how many abstracts do you have to do in each session including the baseline abstracts?"
           '(("1" . "0 today and 25 in the second session.")
             ("2" . "15 today and 25 in the second session.")
             ("3" . "15 today and 40 in the second session.")
             ("4" . "25 today and 25 in the second session.")))))
        submit-button))
        (lambda (#:when-paid when-paid
                 #:session1-payment session1-payment
                 #:how-many-abstracts how-many-abstracts)
          (define score
            (apply
             +
             (map
              (λ (b) (if b 1 0))
              (list
               (string=? when-paid "3")
               (string=? session1-payment "2")
               (string=? how-many-abstracts "3")))))
          (put 'attempt (add1 (get 'attempt)))
          (put 'comprehension-test-score score)))))))

(define max-attempts 3)

(define (repeat-comprehension-test)
  (define next-attempt
    (get 'attempt))
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



(define (no-consent-ending)
  (page
   (haml
    (.container
     (:h1 "Your Completion Code")

     (:p (format "To complete the introduction, go now to prolific and enter the following completion code: ~a" (get/instance 'completion-code)))

     (:p "Since you did not agree to participate in the remainder of the study, you are now done.")))))

(define (thank-you)
  (page
   (haml
    (.container
     (:h1 "Thank you!")

     (:p "Thank you for having participated in the study.")))))

;;;;;;;;;;;;;;;; INTRO STUDY

(define (landing-page)
  (page
   (haml
    @.container{
      @:h1{The Study}

      @:p{Thank you for participating in this study examining how people make decisions for work over time.}

      @:p{This study is conducted by Flora Drucker and Marc Kaufmann and financed by Central European University. Your participation is voluntary and if you accept to participate, you may withdraw at any time. However, please note that you will receive some bonuses if you complete specific parts of the study. This is described in more detail on the Instructions page.}

      @:p{Participation in this study is not associated with any foreseeable risk or benefit. Your answers will be collected confidentially and anonymously (the researchers will not be able to link decisions and participants' identity beyond the Prolific ID provided). At the data analysis stage your Prolific ID will be changed to a random identifying number, and the Prolific IDs will be deleted. In case the results of the study are published, there can be no references to your identity. Data anonymity is guaranteed.}

      @:p{This study received a research ethics approval from the Ethical Research Committee of Central European University.}

      @:p{If you have any questions or concerns regarding this study, please contact us at @"admin@totalinsightmanagement.com" or @"lucafloradrucker.research@gmail.com".}

      @(formular
       (haml
        (:div
         (#:prolific-ID (input-text "What is your Prolific ID?"))
         (#:patience
          (input-number
           "How willing are you to give up something that is beneficial for you today in order to benefit more from that in the future? (0 means not willing at all, 10 means very willing)"
           #:min 0 #:max 10))

         submit-button)))})))

(define (signup)
  (page
   (haml
    (.container
     (:h1 "Sign up for Login")

     (:p "Since part of the study is today and some in the future, you will need to sign up with your Prolific email, so that you can resume the study at any time without losing your progress. To do so, please follow the following steps:")

     (:ul
      (:li "Go to the " (:a ([:href "https://identity.totalinsightmanagement.com"]) "Signup Page"))
      (:li "Sign up for an account with your prolific email Type in your prolific email (it is your Prolific ID followed by ...)")
      (:li "Type in a password")
      (:li "You will receive a validation email on Prolific. Click on or copy-paste the validation link therein to validate your account")
      (:li "Log in to your account and start the study <study-name>"))

     ; FIXME: Add an instructions video on how to do it.

     ))))

(define edpb-intro
  (make-study
   "edpb pilot"
   #:transitions
   (transition-graph
    [landing-page --> signup --> signup])

   (list
    (make-step 'landing-page landing-page)
    (make-step 'signup signup))))

;;; MAIN STUDY

(define (comprehension-test-success?)
  (> (get 'comprehension-test-score) 2))

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
    [initialize --> instructions
                --> task-description
                ; TODO: Do we really want tutorial tasks? The more uncertainty, the better maybe.
                --> tutorial-tasks
                --> comprehension-test
                --> ,(lambda ()
                       (cond [(comprehension-test-success?)
                              (put-current-round-name "")
                              done]

                             [(<= (get 'attempt) max-attempts)
                              (goto repeat-comprehension-test)]

                             [else
                              (put-current-round-name "")
                              (goto fail-comprehension-test)]))]

    [repeat-comprehension-test --> comprehension-test]
    [fail-comprehension-test --> fail-comprehension-test])

   (list
    (make-step 'initialize initialize)
    (make-step 'instructions instructions)
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

;;;;; DAY 1

;;; SINGLE CHOICE
;;;
;;; All choices have two options.
;;; Each option has one of two dates: day1 or day2
;;; Each option has:
;;;     - a type of task, e.g. abstract categorization with subtype "artificial intelligence" or so on
;;;     - an amount of tasks, e.g. 25
;;; For each option, we need to be able to generate the option description

;;; For now, create options and choices for abstract categorization only. Generalize later.
(serializable-struct option [session type amount]
  #:transparent)

; o+r is an option + optional reason
(serializable-struct o+r [option reason]
  #:transparent)

; choice-set consists in an option A and an option B -- without reasons!
(serializable-struct choice-set [A B]
  #:transparent)

; choice-env determines whether there are reasons for each option.
; So A and B are an option + [Maybe reason]
(serializable-struct choice-env [A B]
  #:transparent)

; reasons have a direction ('for or 'against) and some text.
(serializable-struct reason [dir text]
  #:transparent)

(define/contract (describe opt ce)
  (-> (or/c 'A 'B) choice-env? string?)
  (match-define (choice-env (o+r A RA)
                            (o+r B RB))
    ce)
  (define-values (o r)
    (case opt
      [(A) (values A RA)]
      [(B) (values B RB)]))
  (match o
    [(option session type amount)
     ; FIXME: update to have the type.
     (format "~a tasks ~a" amount (if (equal? session 'session1) "today" "next session"))]))

(define/contract (abstract-choice ce)
  ; FIXME: does page return an xexpr?
  (-> choice-env? any/c)
  (define (put/choice o)
    (define choices
      (get 'work-choices '()))
    (put 'work-choices
         (cons (cons o ce) choices)))
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (:div
         (#:choice (radios
                    "Choose between the following two options:"
                    `(("A" . ,(describe 'A ce))
                      ("B" . ,(describe 'B ce)))))
         submit-button)
        ; FIXME: replace this by a flexible radio table with buttons for reasons
        ))
      (lambda (#:choice choice)
        (put/choice choice)))))))

; TODO: Is this better or using rounds to store choice pages? Recursion or for loop?
(define work-choices
  (make-study
   "work choices"
   #:transitions
   (transition-graph
    [choice-page --> ,(lambda ()
                        (define remaining-choices
                          (cdr (get 'remaining-choices)))
                        (put 'remaining-choices remaining-choices)
                        (cond [(null? remaining-choices)
                               next]
                              [else
                               (goto choice-page)]))])
   #:requires '(remaining-choices)
   (list
    (make-step 'choice-page (lambda ()
                              (define next-ce
                                (car (get 'remaining-choices)))
                              (abstract-choice next-ce))))))

(define (set-treatments)
  ; FIXME: Needs to be determined once work choices etc display properly
  (put 'choices-to-make
       (list

        (choice-env
         (o+r (option 'session1 '("Equality" "Other") 25) (reason 'for "'tis good"))
         (o+r (option 'session1 '("Gender" "Other") 25) (reason 'against "dis BAD!")))

        (choice-env
         (o+r (option 'session2 '("Sport" "Other") 15) (reason 'for "Y not!"))
         (o+r (option 'session1 '("Sport" "Other") 20) #f))))
  (skip))

(define day1
  (make-study
   "edpb day 1"
   #:transitions
   (transition-graph
    [set-treatments --> work-choices
                    --> determine-choice-that-counts
                    --> work-day1
                    --> schedule-reminder-email
                    --> ,(lambda () done)])
   (list
    (make-step 'set-treatments set-treatments)
    (make-step/study
     'work-choices
     work-choices
     #:require-bindings '([remaining-choices choices-to-make]))
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
         (put* 'role 'admin)
         (skip)]
        [else
         (put* 'role 'participant)
         (skip)]))

(define (study-open?)
  (equal? (get/instance* 'phase #f) 'open))

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
       (haml
        (:div
         (:h3 "Abstracts")
         (button void "Setup study" #:to-step-id 'abstracts-admin))))

     (:h3 "Completion Code")

     (cond [(get/instance 'completion-code #f)
            => (lambda (c)
                 (haml (:p "The current completion code is " c)))]
           [else
            (haml (:p "No completion code is set."))])

     (button void "Change Completion Code" #:to-step-id 'completion-code-admin)))))

(define (completion-code/admin)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:completion-code (input-text "What is the new completion code?"))
        submit-button))
      (lambda (#:completion-code completion-code)
        (put/instance 'completion-code completion-code)))

     (button void "Cancel")))))

(define (switch-phase-to p #:check-current-phase [cp #f])
  (define old-phase (get/instance* 'phase #f))
  (cond [(or (not cp) (and cp (equal? cp old-phase)))
         (put/instance* 'phase p)]

        [else
         (error 'switch-phase-to "failed because the current phase is ~a, but needs to be ~a to switch phase" old-phase cp)]))

(define (open-study-if-ready)
  (when (and (get/instance 'completion-code #f)
             (get/instance 'abstracts-set? #f))
    (switch-phase-to 'open #:check-current-phase #f)))

(define (consent-end-introduction)
  (page
   (haml
    (.container
     (:h1 "Completion Code for Introduction")

     (:p "You have completed the introduction. Please enter the following completion code on Prolific now before continuing:"
         (get/instance 'completion-code))

     (:p "Once you have done so, start the main study as soon as possible, if you wait too long, you may not be able to participate.")

     (formular
      (haml
       (:div
        (#:completion-code-entered (checkbox "I have entered my completion code on Prolific (required to continue)"))
        submit-button)))))))



(define edpb-main
  (make-study
   "edpb main study"
   #:transitions
   (transition-graph
    ; ROLE ASSIGNMENT
    [assigning-roles --> ,(lambda ()
                            (let ([role (get 'role)])
                              (cond [(equal? role 'admin)       (goto admin)]
                                    [(equal? role 'participant) (goto waiting-page)]
                                    [else                       (goto error-page)])))]
    [error-page --> error-page]

    ; ADMIN
    [admin --> admin]
    [abstracts-admin --> ,(lambda ()
                            (put/instance 'abstracts-set? #t)
                            (open-study-if-ready)
                            (goto admin))]
    [completion-code-admin --> ,(lambda ()
                                  (open-study-if-ready)
                                  (goto admin))]

    ; PARTICIPANT
    [waiting-page --> tutorial
                  --> consent
                  --> ,(lambda ()
                         (cond [(get 'consent-given?) (goto consent-end-introduction)]
                               [else (goto no-consent-ending)]))]

    [no-consent-ending --> no-consent-ending]

    [consent-end-introduction --> day1
                              --> day2
                              --> send-completion-email
                              --> final
                              --> final])

   (list
    (make-step 'assigning-roles assigning-roles)
    (make-step/study 'tutorial (tutorial))
    (make-step 'admin admin)
    (make-step 'completion-code-admin completion-code/admin)
    (make-step/study 'abstracts-admin abstracts-admin)
    (make-step 'waiting-page waiting-page)
    (make-step 'error-page (stub "Error page"))
    (make-step 'consent consent)
    (make-step 'no-consent-ending no-consent-ending)
    (make-step 'consent-end-introduction consent-end-introduction)
    (make-step 'admin admin)
    (make-step/study 'abstracts-admin abstracts-admin)
    (make-step/study 'day1 day1)
    (make-step/study 'day2 day2)
    (make-step 'send-completion-email (stub "Send Completion Email"))
    (make-step 'final final))))
