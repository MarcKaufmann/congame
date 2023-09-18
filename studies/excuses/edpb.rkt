#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/generic
         racket/list
         racket/match
         racket/math
         racket/pretty
         racket/random
         racket/serialize
         (only-in xml xexpr/c)
         koyo/haml
         koyo/url
         congame/components/export
         congame/components/for-study
         congame/components/study
         congame/components/transition-graph
         congame/components/formular
         (submod congame/components/formular tools)
         (submod congame/components/study accessors)
         "templates.rkt"
         "abstract-categorization.rkt")

; TODO:
; - create interface to elicit $ amount needed to equalize the two options, use + and - buttons to do so.
; - update explanations to explain the $ amount an +/-
; - fix decision-that-counts: it seems broken and doesn't allow the new numerical choices with money
; - Finalize choices and randomizations
; - Create list of reasons that I can use and that can be linked to the appropriate choices
;
; Check TODO:
; - Add screenshot of decision choice with reasons, and explain
; - Add feedback section at end, before payment page
; - When announcing tasks, state into which category, display somewhere
; - Rename debriefing to final page/payment page
; - Track the reasons being selected!!!!
; - After consent page, display the code and tell them to immediately start the study
; - Check that instructions answer comprehension questions
; - Use "banking" in the tutorial; use "social preferences" as the baseline tasks to work on, since I didn't collect data on it in the pilot, so we can't have reasons for it.
; Optional:
; - Add definition of categories as written by chatgpt
; - Rate reasonableness of reasons
; - Add debriefing questions on justifying choices
; - Implement a feature to track time used on individual steps and substudies.
; - Implement feature to track progress through the study

(provide
 edpb-intro
 edpb-main
 edpb-pilot)

;;;;;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;;
;;;    Steps that are HTML only      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(define (fail-comprehension-test)
  (page
   (haml
    (.container
     @:h1{You failed the comprehension test too many times}

     @:p{Unfortunately, you failed the comprehension test too many times, so you cannot continue.}

     @:p{Provide the following completion code to receive £@(hash-ref edpb-config 'pilot-fail-comprehension-fee): FAILCOMP.}))))


(define (no-consent)
  (page
   (haml
    (.container
     (:h1 "Thanks for considering the study")

     (:p "Please enter the following completion code on prolific:")

     (:h4 (hash-ref edpb-config 'pilot-completion-code))

     (:p "Since you did not agree to participate in the study, you are done.")))))

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

; NOTE: This is for a version where we explain how to sign up which I might resuscitate later.
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

(define (comprehension-test-success? n)
  (>= (get 'comprehension-test-score) n))


(define (comprehension-test-study comprehension-test n-for-passing)

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

  (make-study
   "comprehension test"
   #:transitions
   (transition-graph
    [comprehension-test --> ,(lambda ()
                               (cond [(comprehension-test-success? n-for-passing)
                                      (put-current-round-name "")
                                      (put 'pass-test? #t)
                                      done]

                                     [(<= (get 'attempt) max-attempts)
                                      (goto repeat-comprehension-test)]

                                     [else
                                      (put-current-round-name "")
                                      (put 'pass-test? #f)
                                      done]))]
    [repeat-comprehension-test --> comprehension-test])

   #:provides '(pass-test?)
   (list
    (make-step 'comprehension-test comprehension-test)
    (make-step 'repeat-comprehension-test repeat-comprehension-test))))

(define (tutorial)
  (make-study
   "edpb-tutorial"
   #:transitions
   (transition-graph
    [instructions --> initialize
                  --> task-description
                  --> tutorial-tasks
                  --> comprehension-test
                  --> ,(lambda ()
                         (if (get 'pass-comprehension-test?)
                             done
                             (goto fail-comprehension-test)))]

    [fail-comprehension-test --> fail-comprehension-test])

   (list
    (make-step 'instructions instructions)
    (make-step 'initialize
               (lambda ()
                 (put 'tutorial-example/in (car (random-abstracts/topic 1 "banking")))
                 (put 'tutorial-example/out (car (random-abstracts/non-topic 1 "banking")))
                 (skip)))
    (make-step
     'task-description
     (lambda ()
       (abstract-examples
        (get 'tutorial-example/in)
        (get 'tutorial-example/out))))
    (make-step/study
     'tutorial-tasks
     (do-abstracts (hash-ref edpb-config 'tutorial-abstracts) "banking" "tutorial" "Tutorial Tasks")
     #:provide-bindings '([correct-tutorial-tasks correct-answers]))
    (make-step/study
     'comprehension-test
     (comprehension-test-study comprehension-test 3)
     #:provide-bindings '([pass-comprehension-test? pass-test?]))
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
(serializable-struct option [session topics amount]
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr o)
     (match-define (option session topics amount) o)
     (hasheq 'type      "option"
             'session   (->jsexpr/super session)
             'topics    (->jsexpr/super topics)
             'amount    (->jsexpr/super amount)))])

; o+r is an option + optional reason
(serializable-struct o+r [option reason]
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr o)
     (match-define (o+r opt r) o)
     (hasheq 'type      "option + reason"
             'option    (->jsexpr/super opt)
             'reason    (->jsexpr/super r)))])

; choice-set consists in an option A and an option B -- without reasons!
(serializable-struct choice-set [A B]
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr cs)
     (hasheq 'type "choice set"
             'option-A (->jsexpr/super (choice-set-A cs))
             'option-B (->jsexpr/super (choice-set-B cs))))])

; choice-env determines whether there are reasons for each option.
; So A and B are an option + [Maybe reason]
(serializable-struct choice-env [A B]
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr ce)
     (hasheq 'type "choice environment"
             'option+reason-A (->jsexpr/super (choice-env-A ce))
             'option+reason-B (->jsexpr/super (choice-env-B ce))))])

; reasons have a direction ('for or 'against) and some text.
(serializable-struct reason [dir text]
  #:transparent
  #:methods gen:jsexprable
  [(define/generic ->jsexpr/super ->jsexpr)
   (define (->jsexpr r)
     (hasheq 'type "reason"
             'direction (->jsexpr/super (and r (reason-dir r)))
             'text      (->jsexpr/super (and r (reason-text r)))))])

(define/contract (describe-abstracts ce k)
  (-> choice-env? (or/c 'A 'B) string?)
  (match-define (choice-env (o+r A RA)
                            (o+r B RB))
    ce)
  (define-values (o r)
    (case k
      [(A) (values A RA)]
      [(B) (values B RB)]))
  (match o
    [(option session type amount)
     (format "Categorize ~a abstracts ~a into '~a' or 'Other'."
             amount
             (if (equal? session 'session1) "today" "next session")
             (cond [(string=? (car type) "ai") "AI"]
                   [else
                    (string-titlecase (car type))]))]))

(define (ce-reason ce label)
  (case label
    [(A) (o+r-reason (choice-env-A ce))]
    [(B) (o+r-reason (choice-env-B ce))]))

(define (abstract-choice/reason ce)
  (define (put/reason label text)
    (define reasons
      (get 'reasons '()))
    (put 'reasons (cons (list label text ce) reasons)))

  (define reasons?
    (or (ce-reason ce 'A)
        (ce-reason ce 'B)))

  (cond [reasons?
         (page
          (haml
           (.container
            (:h3 "Description of Options")

            ,@(for/list ([label '(A B)])
                (define r (ce-reason ce label))
                (haml
                 (:div
                  (:h4 (format "Option ~a" label))

                  (:p (describe-abstracts ce label))

                  )))

            (:h3 "Reveal a Reason")

            ,@(for/list ([label '(A B)]
                         #:when (ce-reason ce label))
                (define r (ce-reason ce label))
                (when (ce-reason ce label)
                  (haml
                   (.reveal-reasons
                    (button
                     (lambda ()
                       (put/reason label (reason-text r))
                       (skip))
                     (format "Reveal a reason ~a Option ~a"
                             (reason-dir r) label)))))))))]

        [else
         (put/reason #f "")
         (skip)]))

(define (abstract-choice ce)
  (define last-reason
    (car (get 'reasons)))
  (define (put/choice o)
    (define choices
      (get 'work-choices '()))
    (put 'work-choices (cons (list (string->symbol o) ce) choices)))
  (define r-label
    (car last-reason))
  (define r-text
    (cadr last-reason))
  (page
   (haml
    (.container

     (:h3 "Description of Options")

     ,@(for/list ([label '(A B)])
         (haml
          (:div
           (:h4 (format "Option ~a" label))

           (:p (describe-abstracts ce label)))))

     (when r-label
       (haml
        (.revealed-reason
         (:h3 (format "Reason for ~a: " r-label))
         (:p r-text))))

     (formular
      (haml
       (:div
        (#:choice
         (radios "Choose an option:"
          `(("A" . "Option A")
            ("B" . "Option B"))))
        submit-button))
      (lambda (#:choice choice)
        (put/choice choice)))))))

(define (bonus-for-switching k)

  (define last-choice
    (car (get 'work-choices)))
  (define chosen-option
    (car last-choice))
  (define unchosen-option
    (if (string=? chosen-option "A") "B" "A"))
  (define last-ce
    (cdr last-choice))
  (define last-reason
    (car (get 'reasons)))
  (define r-label
    (car last-reason))
  (define r-text
    (cadr last-reason))

  (page
   (haml
    (.container

     (:h3 "Description of Options")

     ,@(for/list ([label '(A B)])
         (define r (ce-reason last-ce label))
         (haml
          (:div
           (:h4 (format "Option ~a" label))

           (:p (describe-abstracts last-ce label)))))

     (when r-label
       (haml
        (.revealed-reason
         (:h3 (format "Reason for ~a: " r-label))
         (:p r-text))))

     (:h3 "Bonus needed for switching")

     (:p (format "Between the two above options, you picked Option ~a. Please use the '+' and '-' buttons below to pick the smallest amount of money such that you would choose the other alternative, Option ~a. That is, if we offer you the choice between Option ~a for no bonus payment vs Option ~a with that bonus payment, you'd prefer the latter."
                 chosen-option
                 unchosen-option
                 chosen-option
                 unchosen-option))

     (formular
      (haml
       (:div
        (#:bonus
         (input-number "Switching Bonus" #:min 0.1 #:max 1.0 #:step 0.1))
        submit-button))
      (lambda (#:bonus bonus)
        (with-study-transaction
          (define choices
            (get 'work-choices))
          (define last-choice
            (car choices))
          (put 'work-choices
               (cons (append last-choice bonus) (cdr choices))))))))))


(define work-choices
  (make-study
   "work choices"
   #:transitions
   (transition-graph
    [reason-page --> choice-page
                 --> ,(lambda ()
                        (define remaining-choices
                          (cdr (get 'remaining-choices)))
                        (put 'remaining-choices remaining-choices)
                        (cond [(null? remaining-choices)
                               next]
                              [else
                               (goto reason-page)]))])
   #:requires '(remaining-choices)
   #:provides '(work-choices)
   (list
    (make-step 'reason-page (lambda ()
                              (define next-ce
                                (car (get 'remaining-choices)))
                              (abstract-choice/reason next-ce)))
    (make-step 'choice-page (lambda ()
                              (define next-ce
                                (car (get 'remaining-choices)))
                              (abstract-choice next-ce)))
    (make-step 'bonus-for-switching bonus-for-switching))))

(define (set-treatments)
  (put 'choices-to-make
       (list

        (choice-env
         (o+r (option 'session1 '("socioeconomic inequality" "other") 5) (reason 'for "'tis good"))
         (o+r (option 'session1 '("self-control" "other") 5) (reason 'against "dis BAD!")))

        (choice-env
         (o+r (option 'session1 '("socioeconomic inequality" "other") 5) #f)
         (o+r (option 'session1 '("self-control" "other") 5) #f))

        (choice-env
         (o+r (option 'session2 '("self-control" "other") 10) (reason 'for "Y not!"))
         (o+r (option 'session1 '("self-control" "other") 15) #f))))
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
     #:require-bindings '([remaining-choices choices-to-make])
     #:provide-bindings '([choices-made work-choices]))
    (make-step 'determine-choice-that-counts  (make-stub "Determine choice that counts"))
    (make-step 'work-day1  (make-stub "Work Day 1"))
    (make-step 'schedule-reminder-email  (make-stub "Schedule reminder email")))))


(define day2
  (make-study
   "edpb day 2"
   #:transitions
   (transition-graph
    [work-day2 --> exit-survey --> ,(lambda () done)])
   (list
    (make-step 'work-day2 (make-stub "Work day 2"))
    (make-step 'exit-survey (make-stub "Exit survey")))))

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

(define (admin-page)
  (page
   (haml
    (.container
     (:h1 "Admin")

     (cond [(study-open?)
            (haml
             (:div
              (:h3 "Status")

              (:p "The study is open.")

              (:h3 "Abstracts")

              (:table
               (:thead
                (:tr
                 (:th "Category")
                 (:th "Abstracts in this category")
                 (:th "Abstracts not in this category")))
               (:tbody
                ,@(for/list ([t (get-topics-stats)])
                    (haml
                     (:tr
                      (:td (string-titlecase (car t)))
                      (:td (number->string (cadr t)))
                      (:td (number->string (caddr t))))))))))]

           [else
            (haml
             (:div
              (:h3 "Abstracts")
              (button void "Setup study" #:to-step-id 'abstracts-admin)))])

     (:h3 "Completion Code")

     (cond [(get/instance* 'completion-code #f)
            => (lambda (c)
                 (haml (:p "The current completion code is " c)))]
           [else
            (haml
             (:div
              (:p "No completion code is set.")
              (button void "Change Completion Code" #:to-step-id 'completion-code-admin)))])))))

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
        (put/instance* 'completion-code completion-code)))

     (button void "Cancel")))))

(define (switch-phase-to p #:check-current-phase [cp #f])
  (define old-phase (get/instance* 'phase #f))
  (cond [(or (not cp) (and cp (equal? cp old-phase)))
         (put/instance* 'phase p)]

        [else
         (error 'switch-phase-to "failed because the current phase is ~a, but needs to be ~a to switch phase" old-phase cp)]))

(define (open-study-if-ready)
  (when (and (get/instance* 'completion-code #f)
             (get/instance* 'abstracts-set? #f))
    (switch-phase-to 'open #:check-current-phase #f)))

(define (consent-end-introduction)
  (page
   (haml
    (.container
     (:h1 "Completion Code for Introduction")

     (:p "You have completed the introduction. Please enter the following completion code on Prolific now before continuing:"
         (get/instance* 'completion-code))

     (:p "Once you have done so, start the main study as soon as possible, if you wait too long, you may not be able to participate.")

     (formular
      (haml
       (:div
        (#:completion-code-entered (checkbox "I have entered my completion code on Prolific (required to continue)"))
        submit-button)))))))

(define admin-study
  (make-study
   "abstracts-admin"
   #:transitions
   (transition-graph
    [admin --> admin]
    [abstracts-admin --> ,(lambda ()
                            (put/instance* 'abstracts-set? #t)
                            (open-study-if-ready)
                            (goto admin))]
    [completion-code-admin --> ,(lambda ()
                                  (open-study-if-ready)
                                  (goto admin))])

   (list
    (make-step 'admin admin-page)
    (make-step 'completion-code-admin completion-code/admin)
    (make-step/study 'abstracts-admin abstracts-admin))))

(define (route-participants)
  (let ([role (get 'role)])
    (case role
      [(admin) 'admin]
      [(participant) 'waiting-page]
      [else 'error-page])))

(define (error-page)
  (page
   (haml
    (.container
     (:h1 "An error occurred")

     (:p "We are sorry, but an error occurred. You can contact us to report the error.")))))

;;;;;;;;;; EDPB Pilot to calibrate choices, test reasons, etc

(define (make-option category n [a-reason #f] [dir #f])
  (o+r (option 'session1 `(,category "other") n)
       (and a-reason (reason dir a-reason))))

(define (make-option/for category n a-reason)
  (make-option category n a-reason 'for))

(define (make-option/against category n a-reason)
  (make-option category n a-reason 'against))

(define (set-pilot-choices)
  (put 'choices-to-make
       (list

        (choice-env
         (make-option/for "social preferences" 25 "Reason for")
         (make-option/for "banking" 20 "Reason for"))

        (choice-env
         (make-option/for "socioeconomic inequality" 5 "'tis good")
         (make-option/against "gender inequality" 5 "dis bAD!"))

        (choice-env
         (make-option "socioeconomic inequality" 5)
         (make-option "gender inequality" 5))

        (choice-env
         (make-option/for "self-control" 15 "Y not!")
         (make-option "self-control" 10))))
  (skip))

(define (determine-pilot-choices)
  ; FIXME: Add option to choose the numerical choice with money
  ; FIXME: This is some ugly and tedious code.
  (define cs (get 'choices-made))
  (define c (random-ref cs))
  (define option-chosen (car c))
  (define ce (cadr c))
  (match-define (choice-env (o+r A RA)
                            (o+r B RB))
    ce)
  (define-values (o r)
    (if (equal? option-chosen 'A)
        (values A RA)
        (values B RB)))
  (put 'additional-option o)
  (put 'additional-reason r)
  (match-define (option _session (list category non-category) n)
    o)
  (put 'additional-n n)
  (put 'additional-category category)
  (put 'additional-non-category non-category)
  (page
   (haml
    (.container
     (:h1 "The choice that counts")

     (:p "The choice that matters was between these two options:")

     (:ul
      (:li (format "Option A: ~a" (describe-abstracts ce 'A)))
      (:li (format "Option B: ~a" (describe-abstracts ce 'B))))

     (:p (format "You chose Option ~a, so after doing the baseline tasks, you will categorize ~a additional abstracts based on whether they fit into '~a' or 'Other'."
                 option-chosen
                 n
                 (string-titlecase category)))

     (button void "Continue")))))

; NOTE: This assumes that all choices are of the form "Category" vs
; "Other" (i.e. "Not Category") Generalize if we ever need it.
(define (sample-work-abstracts cat n)
  (define cat-proportion 0.3) ; proportion of abstracts in the category
  (define n-cat (exact-round (* n cat-proportion)))
  (shuffle
   (append
    (random-abstracts/topic n-cat cat)
    (random-abstracts/non-topic (- n n-cat) cat))))

(define ((do-abstracts n category prefix batch-name))
  (define (~prefix s)
    (string->symbol (string-append prefix "-" s)))
  (define abstracts
    (cond [(get (~prefix "abstracts") #f)
           => values]
          [else
           (define new-abstracts
             (sample-work-abstracts category n))
           (put (~prefix "abstracts") new-abstracts)
           new-abstracts]))
  (put (~prefix "n") n)
  (put (~prefix "category") category)
  (define total (length abstracts))
  (for/study ([(abs-task i) (in-indexed (in-list abstracts))])
    (put-current-round-name (format "abstract ~s" i))
    (abstract-task/page abs-task i total category prefix batch-name)))

(define ((display-correct-answers lop n-total))
  (define score
    (for/sum ([p (in-list lop)])
      (get/abstracts* (string->symbol (format "~a-correct-answers" p)) 0)))
  (put* 'abstract-task-score score)
  (page
   (haml
    (.container
     (:h1 (format "You categorized ~a out of ~a abstracts correctly" score n-total))
     (button void "Continue")))))

;;;;;;;; PILOT

(define n-pilot-tutorial
  (hash-ref edpb-config 'pilot-tutorial-abstracts))
(define n-pilot-baseline
  (hash-ref edpb-config 'pilot-baseline-abstracts))

(define (reasons-debrief)
  (page
   (haml
    (.container
     (:h1 "Feedback and Reasons")

     (formular
      (haml
       (:div
        (:div
         (#:how-choose-reason
          (textarea "Please explain how you decided which reasons to reveal.")))
        (:div
         (#:how-much-did-reasons-affect-choices
          (input-likert "How much do you feel that the reasons affected your choices? (1: not at all, 4: somewhat, 7: extremely)")))
        (:div
         (#:how-meaningful-reasons
          (input-likert "How meaningful did you find the reasons? (1: not at all, 4: somewhat, 7: extremely)")))
        (:div
         (#:feedback
          (textarea "Please provide some feedback on what kinds of reasons might have swayed your choice in the kind of situation we gave you, or what type of similar choice (where you are asked to do work for someone else) could sway you. This can include reasons relying brought forward by other people such as colleagues, friends, or spouses.")))
        submit-button)))))))

(define pilot-main
  (make-study
   "main part of pilot"
   #:transitions
   (transition-graph
    [set-choices --> choices
                 --> determine-choice-that-counts
                 --> reasons-debrief
                 --> do-baseline-work
                 --> do-additional-work
                 --> display-total-correct-answers
                 --> ,(lambda () next)])
   (list
    (make-step 'set-choices set-pilot-choices)
    (make-step/study
     'choices
     work-choices
     #:require-bindings '([remaining-choices choices-to-make])
     #:provide-bindings '([choices-made work-choices]))
    (make-step 'determine-choice-that-counts determine-pilot-choices)
    ; FIXME: change require bindings to be an option, not three separate values.
    (make-step 'reasons-debrief reasons-debrief)
    (make-step/study
     'do-baseline-work
     (do-abstracts n-pilot-baseline "social preferences" "baseline" "Baseline Work"))

    (make-step/study
     'do-additional-work
     (lambda ()
       ((do-abstracts
        (get 'additional-n)
        (get 'additional-category)
        "additional-work"
        "Additional Work"))))

    (make-step
     'display-total-correct-answers
     (lambda ()
       (define n
         (+ n-pilot-baseline (get 'additional-n)))
       ((display-correct-answers '("baseline" "additional-work") n)))))))

(define (announce-tutorial-tasks)
  (page
   (haml
    (.container
     (:h1 "Do Two Practice Tasks")

     (:p "You will now be asked to categorize two abstracts.")

     (button void "Start Practice Tasks")))))

(define (pilot-tutorial)

  (define (pilot-comprehension-test)
    (define attempt
      (cond [(get 'attempt #f) => values]
            [else (begin0 1
                    (put 'attempt 1))]))
    (page
     (haml
      (.container
       (:h1 (format "Comprehension Test (~a attempt)" attempt))

       (:p "If you need help to answer the questions, reread the study instructions below the test.")

       (formular
        (haml
         (:div
          (:div
           (#:when-paid
            (radios
             "When will you receive the payments?"
             '(("1" . "Immediately after the tutorial.")
               ("2" . "Immediately after the main session.")
               ("3" . "Within three days after the main session.")))))
          (:div
           (#:how-many-abstracts
            (radios
             "Suppose that you chose 'Categorize 25 abstracts into Animal Rights or Other' in the decision that counts. Then how many abstracts do you have to do in total, including the baseline abstracts?"
             '(("1" . " 0 in total")
               ("2" . "15 in total")
               ("3" . "25 in total")
               ("4" . "40 in total")))))
          (:div
           (#:reasons-to-reveal
            (radios
             "Suppose you face a decision with buttons to reveal reasons for each option. Then which of the following is true?"
             '(("1" . "You can choose an option and submit your choice without having revealed a reason.")
               ("2" . "You can choose an option and submit your choice only after revealing exactly one reason.")
               ("3" . "You can choose an option and submit your choice only after revealing both reasons.")
               ("4" . "You can choose an option and submit your choice after revealing any numbe of reasons.")))))
          submit-button))
        (lambda (#:when-paid when-paid
                 #:how-many-abstracts how-many-abstracts
                 #:reasons-to-reveal reasons-to-reveal)
          (define score
            (apply
             +
             (map
              (λ (b) (if b 1 0))
              (list
               (string=? when-paid "3")
               (string=? how-many-abstracts "4")
               (string=? reasons-to-reveal "2")))))
          (put 'attempt (add1 (get 'attempt)))
          (put 'comprehension-test-score score)))


       (pilot-instructions)

       ))))

  (define (introduction)
    (page
     (haml
      (.container

       (:h2 "Tutorial")

       (:p (format "This study consists of a brief (~a mins) tutorial session followed by the main session. The tutorial is meant to familiarize you with the main study, so you can decide whether you want to participate in it."
                   (conf 'pilot-tutorial-duration-estimate)))

       (:h3 "Comprehension Test")

       (:p "After the tutorial, you will have several attempts at a comprehension test about the main study, where we repeat all the relevant information from the tutorial.")

       (:p (format "If you fail the comprehension test, you receive a completion code with which you receive only ~a, and you cannot participate in the main study."
                   ($conf 'pilot-fail-comprehension-fee)))

       (:p (format "If you pass the comprehension test, you receive another completion code with which you receive the baseline fee of ~a. Moreover, you can then decide whether to participate in the main study for bonus payments described later."
                   ($conf 'pilot-tutorial-fee)))

       (formular
        (haml
         (:div
          (:div
           (#:prolific-id
            (input-text "Please provide your Prolific ID.")))

          (:div
           (#:impatience
            (input-impatience)))
          (:div
           (#:risk
            (input-risk)))
          submit-button)))))))

  (make-study
   "pilot tutorial"
   #:transitions
   (transition-graph
    [introduction --> instructions
                  --> initialize
                  --> task-description
                  --> announce-tutorial-tasks
                  --> tutorial-tasks
                  --> display-correct-tutorial-answers
                  --> comprehension-test
                  --> ,(lambda ()
                         (if (get 'pass-comprehension-test?)
                             done
                             (goto fail-comprehension-test)))]
    [fail-comprehension-test --> fail-comprehension-test])

   (list
    (make-step 'introduction introduction)
    (make-step 'instructions
               (lambda ()
                 (page
                  (haml
                   (.container
                    (pilot-instructions)
                    (button void "Continue"))))))

    (make-step 'initialize
               (lambda ()
                 (put 'tutorial-example/in (car (random-abstracts/topic 1 "well-being")))
                 (put 'tutorial-example/out (car (random-abstracts/non-topic 1 "banking")))
                 (skip)))
    (make-step
     'task-description
     (lambda ()
       (abstract-examples
        (get 'tutorial-example/in)
        (get 'tutorial-example/out))))
    (make-step 'announce-tutorial-tasks announce-tutorial-tasks)
    (make-step/study
     'tutorial-tasks
     (do-abstracts n-pilot-tutorial "banking" "tutorial" "Tutorial Tasks"))
    (make-step 'display-correct-tutorial-answers
               (display-correct-answers '("tutorial") n-pilot-tutorial))
    (make-step/study
     'comprehension-test
     (comprehension-test-study pilot-comprehension-test 3)
     #:provide-bindings '([pass-comprehension-test? pass-test?]))
    (make-step 'fail-comprehension-test fail-comprehension-test))))


(define (consent-show-code)
  (page
   (haml
    (.container
     (:h1 "Enter Completion Code on Prolific")

     (:p "Since you agreed to participate in the main study, do not close this window.")

     (:p "But before continuing, enter the completion code on prolific. This entitles you to the baseline fee, the payments for the main study will be paid out as bonuses later on.")

     (:h4 "Completion Code: " (hash-ref edpb-config 'pilot-completion-code))

     (formular
      (haml
       (:div
        (#:entered-completion-code?
         (checkbox "Did you enter the completion code on Prolific? You cannot proceed until you have done so."))
        submit-button)))))))


(define (debriefing)
  (page
   (haml
    (.container
     (:h1 "Final Survey")

     (formular
      (haml
       (:div
        (:div
         (#:how-choose
          (textarea "Please explain how you chose between the two options:")))
        (:div
         (#:feedback
          (textarea "Please provide any feedback or comments you may have, in particular if there was something that become clear to you as you went through the study and that we should highlight earlier.")))

        submit-button)))))))

(define (payment-page)
  (define score
    (get* 'abstract-task-score))
  (define abstract-bonus
    (* score (hash-ref edpb-config 'pilot-correct-abstract-bonus)))
  (define completion-bonus
    (hash-ref edpb-config 'pilot-completion-fee))
  (define baseline-fee
    (hash-ref edpb-config 'pilot-tutorial-fee))
  (page
   (haml
    (.container
     (:h1 "Thank you for participating")

     (:p "You have completed the study.")

     (:h3 "Your Payment")

     (:ul
      (:li (format "Baseline payment (for tutorial): £~a" (~r baseline-fee #:precision 2)))
      (:li (format "Bonus payment (for main session): £~a" (~r (+ completion-bonus abstract-bonus) #:precision 2))))

     (:p (format "The bonus payment consists of a completion bonus of ~a and of a bonus for categorizing ~a abstracts correctly of ~a."
                 completion-bonus
                 score
                 abstract-bonus))))))


(define edpb-pilot
  (make-study
   "edpb pilot"
   #:transitions
   (transition-graph
    [assigning-roles --> ,route-participants]
    [error-page --> error-page]
    ;; Admin
    [admin --> admin]

    ;; Participant
    [waiting-page --> tutorial
                  --> consent
                  --> ,(lambda ()
                         (if (get 'consent-given?) 'consent-show-code 'no-consent))]

    [no-consent --> no-consent]
    [consent-show-code --> main
                       --> debriefing
                       --> payment-page]
    [payment-page --> payment-page])

   (list
    (make-step 'assigning-roles assigning-roles)
    (make-step 'error-page error-page)
    (make-step/study 'tutorial (pilot-tutorial))
    (make-step 'consent consent)
    (make-step 'consent-show-code consent-show-code)
    (make-step/study 'admin admin-study)
    (make-step 'waiting-page waiting-page)
    (make-step/study 'main pilot-main)
    (make-step 'debriefing debriefing)
    (make-step 'payment-page payment-page)
    (make-step 'no-consent no-consent))))

(define edpb-main
  (make-study
   "edpb main study"
   #:transitions
   (transition-graph
    ; ROLE ASSIGNMENT
    [assigning-roles --> ,route-participants]
    [error-page --> error-page]

    ; ADMIN
    [admin --> admin]

    ; PARTICIPANT
    [waiting-page --> tutorial
                  --> consent
                  --> ,(lambda ()
                         (cond [(get 'consent-given?) (goto consent-end-introduction)]
                               [else (goto no-consent-ending)]))]

    [no-consent --> no-consent]

    [consent-end-introduction --> day1
                              --> day2
                              --> send-completion-email
                              --> final
                              --> final])

   (list
    (make-step 'assigning-roles assigning-roles)
    (make-step/study 'tutorial (tutorial))
    (make-step 'waiting-page waiting-page)
    (make-step 'error-page (make-stub "Error page"))
    (make-step 'consent consent)
    (make-step 'no-consent no-consent)
    (make-step 'consent-end-introduction consent-end-introduction)
    (make-step/study 'admin admin-study)
    (make-step/study 'day1 day1)
    (make-step/study 'day2 day2)
    (make-step 'send-completion-email (make-stub "Send Completion Email"))
    (make-step 'final final))))
