#lang at-exp racket/base

(require racket/contract racket/format
         racket/generic
         racket/list
         racket/match
         racket/math
         racket/pretty
         racket/random
         racket/serialize
         web-server/http
         (only-in xml xexpr/c)
         koyo/haml
         koyo/url
         koyo
         (prefix-in tpl: congame-web/components/template)
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
;
; - Check they understand how payments work, run and build that study without the main session, just ask *if* they'd be interested.
; - Write a study where participants have to categorize the abstracts directly, not just "Yes/No", so that I create the data that I need for the later one.
; - Reread the free-form answers: they gave me some ideas of which other questions people might be interested in, and I should run a follow-up study on the reasons.
; - All TBD: Do basic data analysis to check that we collect everything
;     - All feedback questions (how much reasons matter etc, how choose)
;     - Which reasons they picked based on choice characteristics
;     - Which option they picked
;     - Which switching bonuses they picked
; - Streamline tutorial by breaking into several pages
; - Change abstract categorization task so it takes longer
; - Finalize choices and randomizations.
; - Money reason
;
; Other
;
; - Clean up the abstracts: find those with typos and fix them
; - We could have done the whole thing with another task and more meaningful reasons and less time.
; - Use a different topic than Social preferences for baseline, and check that most categorizations don't require just finding that keyword. Probably most do, given how Rumen and Soheila searched. Ergh.
; - Mats didn't like that people click on the reason when there is a single reason. I can probably quickly update to have them simply click "next" at the bottom instead, so I display the page, then they move to the choice page.
; - Use as task the task to fix the spaces between words -- and as a reason state they will actually do some work that we haven't already done, unlike the categorizations
; - Allow only so many wrong abstract categorizations, otherwise it is game over. Do that after a first test run when I see how many they get wrong.
; - Simplify language of reasons further. That way I can simplify the language further, and make it sound less stupid. E.g.: "This topic was among the top 25% most important/controversial/learn more about.
; - Should we add a comprehension question on what the reasons are? Yes, if we add money as a reason.
; - Over two days, there are several other reasons: total work, total additional, total across both days and so on.
; - We don't need tons of reasons, merely better ones, and we need to maybe reduce the number of reasons we ask, at least in the first part?
;
; MINOR TODO:
;
; - Styling and text: highlight surveys by having more margin and padding on them
; - Have a look at how Qualtrics styles things, implement some of that
; - Improve the styling of standard inputs (requires shoelace or similar)
;
; Optional:
; - Generate additional reasons by running a pilot without reasons over two periods and asking participants why they choose one option over the other. Here just provide them with some information from the surveys, and let them pick what they want to justify it.
; - Put category choice button at the same height always, based on normal length of abstracts
; - Add definition of categories as written by chatgpt
; - Rate reasonableness of reasons
; - Add debriefing questions on justifying choices
; - Implement feature to track progress through the study

(provide
 edpb-intro
 edpb-main
 edpb-pilot)

;;;;;;;;;;;;;;;; CHOICES ;;;;;;;;;;;;;;;

(define (set-pilot-choices)
  (define debug-choices
    (list
        ;(choice-env
        ;(make-option/for "social preferences" 50)
        ;(make-option/for "banking" 40))
        (choice-env
         (make-option "social preferences" 2)
         (make-option "banking" 3))))

  (define choices-to-make
       (list

        ; The following was used for the screenshot
        ;(choice-env
        ;(make-option/for "social preferences" 50)
        ;(make-option/for "banking" 40))

        ; 4 choices without reasons to calibrate preferences
        (choice-env
         (make-option "socioeconomic inequality" 50)
         (make-option "covid" 40))

        (choice-env
         (make-option "self-control" 50)
         (make-option "environment" 40))

        (choice-env
         (make-option "neuroscience" 40)
         (make-option "politics" 20))

        (choice-env
         (make-option "cognitive skills" 50)
         (make-option "addiction" 70))

        ; 6 choices with choice of reasons
        ; 3 with reasons for, 3 with reaons against
        (choice-env
         (make-option/for "socioeconomic inequality" 50)
         (make-option/for "covid" 40))

        (choice-env
         (make-option/for "self-control" 40)
         (make-option/for "environment" 30))

        (choice-env
         (make-option/for "cognitive skills" 50)
         (make-option/for "addiction" 30))

        (choice-env
         (make-option/against "neuroscience" 40)
         (make-option/against "politics" 30))

        (choice-env
         (make-option/against "socioeconomic inequality" 30)
         (make-option/against "gender inequality" 50))

        (choice-env
         (make-option/against "neuroscience" 40)
         (make-option/against "gender inequality" 60))

        ; 12 choices with a single exogenous reason or no reason
        ; 4 with reasons for
        ; 4 with reasons against
        ; 4 without reasons
        ;(choice-env
        ; (make-option/for "covid" 20)
        ; (make-option "politics" 5))

        ;(choice-env
        ; (make-option "self-control" 5)
        ; (make-option/for "addiction" 20))

        ;(choice-env
        ; (make-option/for "gender inequality" 30)
        ; (make-option "neuroscience" 45))

        ;(choice-env
        ; (make-option "environment" 5)
        ; (make-option/for "cognitive skills" 20))

        ;(choice-env
        ; (make-option/against "ai" 5)
        ; (make-option "addiction" 15))

        ;(choice-env
        ; (make-option/against "politics" 15)
        ; (make-option "self-control" 5))

        ;(choice-env
        ; (make-option "sports" 5)
        ; (make-option/against "politics" 20))

        ;(choice-env
        ; (make-option "ai" 20)
        ; (make-option/against "gender inequality" 30))

        ;(choice-env
        ; (make-option "environment" 35)
        ; (make-option "neuroscience" 30))

        ;(choice-env
        ; (make-option "sports" 40)
        ; (make-option "gender inequality" 25))

        ;(choice-env
        ; (make-option "socioeconomic inequality" 20)
        ; (make-option "covid" 10))

        ;(choice-env
        ; (make-option "socioeconomic inequality" 35)
        ; (make-option "gender inequality" 25))
        ))

  (put 'choices-to-make choices-to-make)

  (skip))

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
  (define last-attempt
    (get 'last-attempt #f))
  (put-current-round-name (~a "attempt " attempt))
  (define (answer-feedback question option correct-answer?)
    (haml
     (:li "For the question '"
          (:em question)
          (format "' you picked option ~a, which is ~a."
                  option
                  (if correct-answer? "true." "false.")))))
  (page
   (haml
    (.container
     (:h1 (format "Comprehension Test (~a attempt)"
                  (case attempt
                    [(1) "first"]
                    [(2) "second"]
                    [(3) "third"]
                    [(4) "fourth"])))

     (when last-attempt
       (let ([when-paid (hash-ref last-attempt 'when-paid)]
             [session1 (hash-ref last-attempt 'session1)]
             [how-many-abstracts (hash-ref last-attempt 'how-many-abstracts)])
       (haml
        (:div
         (:h4 "Your answers and Score from the previous attempt")

         (:ol
          (answer-feedback "When will you receive the payments" (car when-paid) (cdr when-paid))
          (answer-feedback "Will you receive any payment if you only complete the first session"
                           (car session1) (cdr session1))
          (answer-feedback "how many abstracts do you have to do in each session including the baseline abstracts"
                           (car how-many-abstracts) (cdr how-many-abstracts)))))))

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
          (put 'last-attempt
               (hash 'when-paid (cons when-paid (string=? when-paid "3"))
                     'session1-payment (cons session1-payment (string=? session1-payment "2"))
                     'how-many-abstracts (cons how-many-abstracts (string=? how-many-abstracts "3"))))
          (put 'comprehension-test-score score)))))))

(define max-attempts 4)


(define (fail-comprehension-test)
  (page
   (haml
    (.container
     @:h1{You failed the comprehension test too many times}

     @:p{Unfortunately, you failed the comprehension test too many times, so you cannot continue with the main session.}

     @:p{Provide the following completion code to receive @($conf 'pilot-tutorial-fee): @(conf 'pilot-code-for-failed-comprehension).}))))


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

      @:p{Thank you for participating in this study examining how people make decisions for work.}

      @:p{This study is conducted by Flora Drucker and Marc Kaufmann and financed by Central European University. Your participation is voluntary and if you accept to participate, you may withdraw at any time. However, please note that you will receive some bonuses if you complete specific parts of the study. This is described in more detail on the Instructions page.}

      @:p{Participation in this study is not associated with any foreseeable risk or benefit. Your answers will be collected confidentially and anonymously (the researchers will not be able to link decisions and participants' identity beyond the Prolific ID provided). At the data analysis stage your Prolific ID will be changed to a random identifying number, and the Prolific IDs will be deleted. In case the results of the study are published, there can be no references to your identity. Data anonymity is guaranteed.}

      @:p{This study received a research ethics approval from the Ethical Research Committee of Central European University.}

      @:p{If you have any questions or concerns regarding this study, please contact us at @"admin@totalinsightmanagement.com" or @"lucafloradrucker.research@gmail.com".}

      @(formular
       (haml
        (:div
         (:div
          (#:prolific-ID (input-text "What is your Prolific ID?")))
         (:div
          (#:patience
           (input-patience)))
         (:div
          (#:risk
           (input-risk)))
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

       @:p.info{On your next attempt, make sure to read the feedback at the top, which tells you which questions you got right.}

       @:p{Remember: You can fail the test at most @(~a max-attempts) times, otherwise you cannot participate in the main session. And you have to use all your attempts to receive the completion code. Try again.}

       (button void "Try Again")))))

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
     (do-abstracts (hash-ref edpb-config 'tutorial-abstracts) "banking" "tutorial" "Tutorial Tasks" #:real-stakes? #f)
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
  (-> choice-env? (or/c 'A 'B) any/c)
  (match-define (choice-env (o+r A RA)
                            (o+r B RB))
    ce)
  (define-values (o r)
    (case k
      [(A) (values A RA)]
      [(B) (values B RB)]))
  (match o
    [(option session type amount)
     (haml
      (:div
       (format "Categorize ~a abstracts ~a into "
               amount
               (if (equal? session 'session1) "today" "next session"))

       (haml (:em
              (cond [(string=? (car type) "ai") "AI"]
                    [else
                     (string-titlecase (car type))])))
       " or " (haml (:em "Other")) "."))]))

(define (ce-reason ce label)
  (case label
    [(A) (o+r-reason (choice-env-A ce))]
    [(B) (o+r-reason (choice-env-B ce))]))

(define (abstract-choice/reason ce total i)
  (define (put/reason label text)
    (define reasons
      (get 'reasons '()))
    (put 'reasons (cons (list label text ce) reasons)))

  (define reasons?
    (or (ce-reason ce 'A)
        (ce-reason ce 'B)))

  (cond [reasons?
         (start-timer)
         (page
          (haml
           (.container

            (:h2 (format "Choice ~a out of ~a" i total))
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
                       (put* #:root '*timer*
                             'reveal-reasons-durations
                             (cons (end-timer)
                                   (get* #:root '*timer* 'reveal-reasons-durations null)))
                       (put/reason label (reason-text r))
                       ; FIXME: Isn't this `skip` redundant?
                       (skip))
                     (format "Reveal a reason ~a Option ~a"
                             (reason-dir r) label)))))))))]

        [else
         (put/reason #f "")
         (skip)]))

(define (abstract-choice ce total i)
  (define last-reason
    (car (get 'reasons)))
  (define (put/choice o)
    (define choices
      (get 'work-choices null))
    (put 'work-choices (cons (list (string->symbol o) ce) choices)))
  (define r-label
    (car last-reason))
  (define r-text
    (cadr last-reason))
  (start-timer)
  (page
   (haml
    (.container

     (:h2 (format "Choice ~a out of ~a" i total))

     (:h3 "Description of Options")

     ,@(for/list ([label '(A B)])
         (haml
          (:div
           (:h4 (format "Option ~a" label))

           (:p (describe-abstracts ce label)))))

     (when r-label
       (haml
        (.revealed-reason.info
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
        (put* #:root '*timer*
              'choice-durations
              (cons (end-timer)
                    (get* #:root '*timer* 'choice-durations null)))
        (put/choice choice)))))))

(define (bonus-for-switching total i)

  (define last-choice
    (car (get 'work-choices)))
  (define chosen-option
    (car last-choice))
  (define unchosen-option
    (case chosen-option
      [(A) 'B]
      [(B) 'A]))
  (define last-ce
    (cadr last-choice))
  (define last-reason
    (car (get 'reasons)))
  (define r-label
    (car last-reason))
  (define r-text
    (cadr last-reason))

  (start-timer)
  (page
   (haml
    (.container

     (:h2 (format "Choice ~a out of ~a" i total))
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

     (:p (format "You just picked Option ~a in a direct choice between the above options. Now please pick the smallest amount of money you have to be paid to switch to Option ~a instead."
                 chosen-option
                 unchosen-option))

     (formular
      (haml
       (:div
        (#:bonus
         (input-number
          "Switching Bonus"
          #:min 0.1
          #:max 1.1
          #:step 0.1
          #:attributes `([data-counter "counter"]
                         [data-counter-prefix "$"])))
        submit-button))
      (lambda (#:bonus bonus)
        (with-study-transaction
          (put* #:root '*timer*
                'switch-choice-durations
                (cons (end-timer)
                      (get* #:root '*timer* 'switch-choice-durations null)))
          (define choices
            (get 'work-choices))
          (define last-choice
            (car choices))
          (put 'work-choices
               (cons (append last-choice (list bonus)) (cdr choices))))))))))


(define (work-choices)

  (define (initialize-work-choices)
    (put 'total (length (get 'remaining-choices)))
    (put 'choice-number 1)
    (skip))

  (make-study
   "work choices"
   #:transitions
   (transition-graph
    [initialize --> reason-page
                --> choice-page
                --> bonus-for-switching
                --> ,(lambda ()
                       (define remaining-choices
                         (cdr (get 'remaining-choices)))
                       (put 'remaining-choices remaining-choices)
                       (put 'choice-number
                            (add1 (get 'choice-number)))
                       (cond [(null? remaining-choices)
                              next]
                             [else
                              (goto reason-page)]))])
   #:requires '(remaining-choices)
   #:provides '(work-choices)
   (list
    (make-step 'initialize initialize-work-choices)
    (make-step 'reason-page (lambda ()
                              ; FIXME: I wonder if this 'functional' style is worth it? Why not get the values inside of abstract-choice/reason?
                              (define next-ce
                                (car (get 'remaining-choices)))
                              (abstract-choice/reason next-ce (get 'total) (get 'choice-number))))
    (make-step 'choice-page (lambda ()
                              (define next-ce
                                (car (get 'remaining-choices)))
                              (abstract-choice next-ce (get 'total) (get 'choice-number))))
    (make-step
     'bonus-for-switching
     (lambda ()
       (bonus-for-switching (get 'total) (get 'choice-number)))))))

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
     (work-choices)
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

(define (compute-total-bonus progress)
  (match-define (hash-table ('completed-tutorial? tutorial?)
                            ('pass-comprehension-test? comprehension?)
                            ('completed-main-session? main?)
                            ('correct-abstract-tasks correct-tasks))
  progress)
  (define completion-bonus
    (hash-ref edpb-config 'pilot-completion-fee))
  (define bonus-per-correct-abstract
    (hash-ref edpb-config 'pilot-correct-abstract-bonus))
  (define abstract-bonus
    (* correct-tasks bonus-per-correct-abstract))
  (define total-payment
    (+ (if main? completion-bonus 0)
       abstract-bonus))
  total-payment)

#;(define (admin-payments)
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (tpl:page
      (tpl:container
       (haml
        (:div
         (:h4
          (:a
           ([:href
             (embed/url
              (lambda (_req)
                (define test '(a b c))
                (response/output
                 #:headers (list (make-header #"content-disposition" #"attachment; filename=\"prolific-payments-edpb-pilot.csv\""))
                 (lambda (out)
                   (for ([p (in-list test)])
                     (fprintf out
                              "~a,~a~n"
                              (first p)
                              (~r #:precision '(= 2)
                                  (second p))))))))])
           "Export Bonus Payments CSV"))

         (button void "Admin" #:to-step-id 'admin)

         (:table
          (:thead
           (:tr
            (:td "Prolific ID")
            (:td "Tutorial")
            (:td "Comprehension Test")
            (:td "Consented?")
            (:td "Main Session")
            (:td "Correct Abstracts")
            (:td "Total Abstracts done")
            (:td "Total Bonus")))

          (:tbody
           ,@(for/list ([(id p) (in-hash (get/instance* 'progress (hash)))])
               (haml
                (:tr
                 (:td id)
                 (:td (~a (hash-ref p 'completed-tutorial?)))
                 (:td (~a (hash-ref p 'pass-comprehension-test?)))
                 (:td (~a (hash-ref p 'consented?)))
                 (:td (~a (hash-ref p 'completed-main-session?)))
                 (:td (~a (hash-ref p 'correct-abstract-tasks)))
                 (:td (~a (hash-ref p 'total-abstracts-done)))
                 (:td (~r (compute-total-bonus p) #:precision 2))))))))))))))

(define (admin-page)
  (define progress
    (get/instance* 'progress (hash)))
  (define progress-with-bonus
    (for/hash ([(id p) (in-hash progress)])
      (values id (hash-set p 'bonus (compute-total-bonus p)))))

  (page
   (haml
    (.container
     (:h1 "Admin")

     (when (study-open?)
       (haml
        (:div
         (:h3 "Participant Payments")

         #;(button (lambda ()
                   (put/instance* 'progress (hash)))
                 "Delete progress data"
                 #:to-step-id 'admin)

         (:table
          (:thead
           (:tr
            (:td "Prolific ID")
            (:td "Tutorial")
            (:td "Comprehension Test")
            (:td "Consented?")
            (:td "Main Session")
            (:td "Correct Abstracts")
            (:td "Total Abstracts done")
            (:td "Total Bonus")
            (:td "Time taken for main session")
            (:td "Avg hourly bonus")))

          (:tbody
           ,@(for/list ([(id p) (in-hash progress-with-bonus)])
               (define bonus (hash-ref p 'bonus))
               (define time-taken (hash-ref p 'main-session-time #f))
               (haml
                (:tr
                 (:td id)
                 (:td (~a (hash-ref p 'completed-tutorial?)))
                 (:td (~a (hash-ref p 'pass-comprehension-test?)))
                 (:td (~a (hash-ref p 'consented?)))
                 (:td (~a (hash-ref p 'completed-main-session?)))
                 (:td (~a (hash-ref p 'correct-abstract-tasks)))
                 (:td (~a (hash-ref p 'total-abstracts-done)))
                 (:td (~r bonus #:precision 2))
                 (:td (if time-taken (~r #:precision 1 (/ time-taken 60.0)) "#<NA>"))
                 (:td (if time-taken (~r #:precision 2 (* 3600 (/ bonus time-taken))) "#<NA>")))))))

         (:h4 "Participants who should be approved")

         (:table
          (:thead
           (:tr
            (:td "Id")))
          (:tbody
           ,@(for/list ([(id p) (in-hash progress)]
                        #:when (boolean? (hash-ref p 'pass-comprehension-test?)))
               (haml
                (:tr
                 (:td id))))))

         (:h4 "Participants with Bonus in GBP in Format for Prolific")

         (:table
          (:thead
           (:tr
            (:td "Id,bonus")))
          (:tbody
           ,@(for/list ([(id p) (in-hash progress-with-bonus)]
                        #:when (> (hash-ref p 'bonus) 0))
               (haml
                (:tr
                 (:td
                  (format "~a,~a" id (~r #:precision 2 (* 0.83 (hash-ref p 'bonus)))))))))))))


     (cond [(get/instance* 'abstracts-set? #f)
            (haml
             (:div
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
                      (:td (number->string (caddr t))))))))

              (unless (study-open?)
                (button void "Update Abstrats" #:to-step-id 'abstracts-admin))))]

           [else
            (haml
             (:div
              (:h3 "Abstracts")
              (button void "Setup abstracts" #:to-step-id 'abstracts-admin)))])

     (cond [(get/instance* 'reasons-set? #f)

            (haml
             (:div
              (:h3 "Reasons")

              (:table
               (:thead
                (:tr
                 (:th "Category")
                 (:th "Reasons For")
                 (:th "Reasons Against")))
               (:tbody
                ,@(for/list ([t (get-reasons-stats)])
                    (haml
                     (:tr
                      (:td (string-titlecase (symbol->string (car t))))
                      (:td (number->string (cadr t)))
                      (:td (number->string (caddr t))))))))

              (unless (study-open?)
                (button void "Update Reasons" #:to-step-id 'reasons-admin))))]

           [else
            (haml
             (:div
              (:h3 "Reasons")

              (button void "Setup Reasons" #:to-step-id 'reasons-admin)))])


     (:h3 "Completion Code")

     (cond [(get/instance* 'completion-code #f)
            => (lambda (c)
                 (haml
                  (:div
                   (:p "The completion code is set to " c)

                   (unless (study-open?)
                     (button void "Update completion code" #:to-step-id 'completion-code-admin)))))]

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
             (get/instance* 'abstracts-set? #f)
             (get/instance* 'reasons-set? #f))
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
                            (unless (get 'cancel?)
                              (put/instance* 'abstracts-set? #t))
                            (open-study-if-ready)
                            (goto admin))]

    [completion-code-admin --> ,(lambda ()
                                  (open-study-if-ready)
                                  (goto admin))]
    [reasons-admin --> ,(lambda ()
                          (put/instance* 'reasons-set? #t)
                          (open-study-if-ready)
                          (goto admin))])

   (list
    (make-step 'admin admin-page)
    (make-step 'completion-code-admin completion-code/admin)
    (make-step/study
     'abstracts-admin
     #:provide-bindings '([cancel? cancel?])
     abstracts-admin)
    (make-step 'reasons-admin (reasons-admin #:step-on-cancel 'admin)))))

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


(define (waiting-page-pilot)
  (define id (get 'prolific-ID))
  ; FIXME: This took me longer than it should have.
  ; The problem is that I can't use skip in a transaction, and I need a transaction.
  (define new-user
    (with-study-transaction
      (define progress
        (get/instance* 'progress (hash)))
      (cond [(hash-has-key? progress id)
             #f]
            [else
             (put/instance*
              'progress
              (hash-set progress id
                        (hash 'completed-tutorial? #f
                              'consented? "not yet taken"
                              'pass-comprehension-test? "not yet taken"
                              'completed-main-session? #f
                              'correct-abstract-tasks 0
                              'total-abstracts-done 0
                              'main-session-time #f)))
             #t])))
  (unless new-user
    (skip 'participant-took-study-already))

  (cond [(study-open?)
         (skip)]

        [else
         (page
          (haml
           (.container
            (:h1 "The study is not yet open")

            (:p "The study is not yet open for participants. Please come back later.")
            (:p "If you believe this is in error, please send an email to the study admin."))))]))

(define (make-option category n [a-reason #f] [dir #f])
  (o+r (option 'session1 `(,category "other") n)
       (and a-reason (reason dir a-reason))))

(define (get-random-reason category dir)
  (random-ref (hash-ref
               (get/instance* (case dir
                                [(for) 'reasons-for]
                                [(against) 'reasons-against]))
               (if (string? category)
                   (string->symbol category)
                   category))))

(define (make-option/for category n)
  (define a-reason
    (get-random-reason category 'for))
  (make-option category n a-reason 'for))

(define (make-option/against category n)
  (define a-reason
    (get-random-reason category 'against))
  (make-option category n a-reason 'against))

(define (determine-pilot-choices)
  ; FIXME: separate determinining the choice and displaying, or else F5 chooses a new option.
  (define cs (get 'choices-made))
  (define c (random-ref cs))
  (define option-chosen (car c))
  (define ce (cadr c))
  (define switching-bonus (caddr c))
  (define type-that-counts (random-ref '(binary numeric)))
  (define bonus-that-counts
    (random-ref (range 0.1 1.05 0.1)))
  (match-define (choice-env (o+r A RA)
                            (o+r B RB))
    ce)
  (define-values (chosen r-chosen unchosen r-unchosen)
    (if (equal? option-chosen 'A)
        (values A RA B RB)
        (values B RB A RA)))
  (put 'choice-that-counts
       (hash 'ce ce
             'type type-that-counts
             'switching-bonus switching-bonus
             'bonus-that-counts bonus-that-counts
             'chosen-label option-chosen
             'chosen chosen
             'unchosen unchosen
             'chosen-reason r-chosen
             'unchosen-reason r-unchosen))
  (skip))

(define (display-choice-that-counts)
  (define choice-that-counts (get 'choice-that-counts))
  (match-define (hash-table
                 ('ce ce)
                 ('type type-that-counts)
                 ('switching-bonus switching-bonus)
                 ('bonus-that-counts bonus-that-counts)
                 ('chosen-label chosen-label)
                 ('chosen chosen)
                 ('unchosen unchosen)
                 ('chosen-reason r-chosen)
                 ('unchosen-reason r-unchosen))
    choice-that-counts)
  (define binary? (equal? type-that-counts 'binary))
  (define-values (option-that-counts r)
    (if (or binary? (< bonus-that-counts switching-bonus))
        (values chosen r-chosen)
        (values unchosen r-unchosen)))
  (match-define (option _session (list category non-category) n)
    option-that-counts)

  (define unchosen-label
    (case chosen-label
      [(A) 'B]
      [(B) 'A]))

  (put 'additional-n n)
  (put 'additional-category category)
  (put 'additional-non-category non-category)

  (page
   (haml
    (.container (:h1 "The choice that counts")

                (:p (format "The choice that matters was between these two options~a:"
                            (if binary? "" " with a switching bonus")))

                (:ul
                 (:li "Option A: " (describe-abstracts ce 'A))
                 (:li "Option B: " (describe-abstracts ce 'B)))


                (cond [binary?

                       (haml
                        (:p (format "You chose Option ~a, so after doing the baseline tasks, you will do the work of Option ~a:" chosen-label chosen-label)))]

                      [(< bonus-that-counts switching-bonus)

                       (haml
                        (:p (format "You chose Option ~a and stated a minimal switching bonus of $~a to switch. The random bonus that was picked is $~a, which is less than the minimal switching bonus. So you will do the work of Option ~a:"
                                    chosen-label
                                    (~r switching-bonus #:precision 2)
                                    (~r bonus-that-counts #:precision 2)
                                    chosen-label)))]

                      [else
                       (haml
                        (:p (format "You chose Option ~a and stated a minimal switching bonus of $~a to switch. The random bonus that was picked is $~a, which is larger than or equal to the minimal switching bonus. So you will do the work of Option ~a and receive an extra bonus of $~a:"
                                    chosen-label
                                    (~r switching-bonus #:precision 2)
                                    (~r bonus-that-counts #:precision 2)
                                    unchosen-label
                                    (~r bonus-that-counts #:precision 2))))])

                (:p (format "Categorize ~a additional abstracts based on whether they fit into '~a' or 'Other'."
                            n
                            (string-titlecase category)))

                #;(button #:to-step-id 'determine-choice-that-counts void "Determine NEW choice that counts (DEBUG)")
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

(define ((do-abstracts n category prefix batch-name #:real-stakes? [real-stakes? #t]))
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
    (abstract-task/page abs-task i total category prefix batch-name #:real-stakes? real-stakes?)))

(define ((display-correct-answers lop n-total))
  (define score
    (for/sum ([p (in-list lop)])
      (get/abstracts* (string->symbol (format "~a-correct-answers" p)) 0)))
  (put* 'abstract-task-score score)
  (put* 'abstract-total-done n-total)
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

(define (participant-took-study-already)
  (page
   (haml
    (.container
     (:h1 "You took the study already")

     (:p "It appears that you already took our study (or you entered your prolific ID wrongly), and you cannot retake this study. Please contact us if you believe this is wrong.")))))

(define (reasons-debrief)
  (page
   (haml
    (.container
     (:h1 "Feedback on Choices and Reasons")
     (:p "Before you do the additional work, please answer the following questions on your decision-making process.")

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
         (#:how-long-abstracts
          (input-number "How many minutes do you guess it would take you to categorize 20 abstracts for a random topic?"
                        #:min 0
                        #:max 75)))
        (:div
         (#:feedback
          (textarea "Please provide some feedback on what kinds of reasons might have swayed your choice in the kind of situation we gave you, or what type of similar choice (where you are asked to do work for someone else) could sway you. This can include reasons relying brought forward by other people such as colleagues, friends, or spouses.")))
        submit-button)))))))

(define (input-likert/how adjective)
  (input-likert
   (format "How ~a did you find the abstracts? (1: not at all. 7: extremely.)" adjective)))

(define (category-survey)
  (define category (get 'additional-category))
  (page
   (haml
    (.container
     (:h1 "Survey on Abstract Categorization")

     (:p (format "Answer the following questions for the additional abstracts in ~a that you just categorized:" category))
     (formular
      (haml
       (:div
        (:div
         (#:boring (input-likert/how "boring")))
        (:div
         (#:understandable (input-likert/how "understandable")))
        (:div
         (#:careful (input-likert "How carefully did you read the abstracts? (1: not at all. 7: extremely.)")))
        submit-button)))))))


(define pilot-main
  (make-study
   "main part of pilot"
   #:transitions
   (transition-graph
    [set-choices --> choices
                 --> determine-choice-that-counts
                 --> display-choice-that-counts
                 --> reasons-debrief
                 --> do-baseline-work
                 --> do-additional-work
                 --> display-total-correct-answers
                 --> category-survey
                 --> ,(lambda () next)])
   (list
    (make-step 'set-choices set-pilot-choices)
    (make-step/study
     'choices
     (work-choices)
     #:require-bindings '([remaining-choices choices-to-make])
     #:provide-bindings '([choices-made work-choices]))
    (make-step 'determine-choice-that-counts determine-pilot-choices)
    (make-step 'display-choice-that-counts display-choice-that-counts)
    ; FIXME: change require bindings to be an option, not three separate values.
    (make-step 'reasons-debrief reasons-debrief)
    (make-step/study
     'do-baseline-work
     (lambda ()
       (define n (+ n-pilot-baseline (get 'additional-n)))
       (put* 'total-tasks-to-do n)
       (put* 'max-wrong-abstracts
             (exact-floor (* 0.33 n)))
       ((do-abstracts n-pilot-baseline "social preferences" "baseline" "Baseline Work"))))

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
       ((display-correct-answers '("baseline" "additional-work") n))))
    (make-step 'category-survey category-survey))))

(define (announce-tutorial-tasks)
  (page
   (haml
    (.container
     (:h1 "Do Two Practice Tasks")

     (:p "You will now be asked to categorize two abstracts.")

     (button void "Start Practice Tasks")))))

(define (progress-update k v)
  (eprintf "progress-update: running with key ~a and value ~v~n~n" k v)
  (define id (get* 'prolific-ID))
  (with-study-transaction
    (define progress
      (get/instance* 'progress))
    (define participant-progress
      (hash-ref progress id))
    (put/instance*
     'progress
     (hash-set
      progress
      id
      (hash-update
       participant-progress
       k (lambda (x) v))))))

(define (pilot-tutorial)

  (define (pilot-comprehension-test)
    (define attempt
      (cond [(get 'attempt #f) => values]
            [else (begin0 1
                    (put 'attempt 1))]))

    (define last-attempt
      (get 'last-attempt #f))
    (define (answer-feedback question option correct-answer?)
      (haml
       (:li "Question: "
            (:em question)
            (format " You picked option ~a, which is ~a."
                    option
                    (if correct-answer? "true" "false")))))

    (page
     (haml
      (.container
       (:h1 (format "Comprehension Test (~a attempt)"
                    (case attempt
                      [(1) "first"]
                      [(2) "second"]
                      [(3) "third"]
                      [(4) "fourth"])))

       (:p (format "You can attempt the comprehension test ~a times. If you need help to answer the questions, reread the study instructions below the test." max-attempts))
       (when last-attempt
         (let ([when-paid (hash-ref last-attempt 'when-paid)]
               [baseline-n (hash-ref last-attempt 'how-many-baseline-abstracts)]
               [reasons (hash-ref last-attempt 'reasons-to-reveal)]
               [how-many-abstracts (hash-ref last-attempt 'how-many-abstracts)])
           (haml
            (.info
             (:h4 "Your answers and Score from the previous attempt")

             (:ol
              (answer-feedback "When will you receive the payments?" (car when-paid) (cdr when-paid))
              (answer-feedback "How many abstracts do you have to do as baseline work?" (car baseline-n) (cdr baseline-n))
              (answer-feedback "Then how many abstracts do you have to do in total, including the baseline abstracts?"
                               (car how-many-abstracts) (cdr how-many-abstracts))
              (answer-feedback "Suppose you face a decision with buttons to reveal reasons for each option. Then which of the following is true?"
                               (car reasons) (cdr reasons)))))))

       (when (> attempt 2)
         (haml
          (.alert
           (:h4 "Hints")

           (:ul
            (:li "Regarding when you receive the payments, see the start of the 'Payments' section at the bottom of the page.")
            (:li "You definitely do have to do some baseline abstracts, see the section on 'Abstract Categorization Tasks'.")
            (:li "Regarding how many abstracts you have to do: note that you have to do a certain amount of baseline abstracts (see the section on 'Abstract Categorization Tasks'). Then you do the abstracts from the choice " (:strong "in addition") " to these baseline abstracts.")
            (:li "Regarding the reasons, see the subsection on 'Revealing Reasons'. When there is a reason, how many do you have to reveal?")))))

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
           (#:how-many-baseline-abstracts
            (radios
             "How many abstracts do you have to do as baseline work before you do the abstracts based on your choice?"
             '(("1" . "10 abstracts")
               ("2" . "30 abstracts")
               ("3" . "50 abstracts")
               ("4" . "None")))))
          (:div
           (#:how-many-abstracts
            (radios
             "Suppose that you chose 'Categorize 50 abstracts into Animal Rights or Other' in the decision that counts. Then how many abstracts do you have to do in total, including the baseline abstracts?"
             '(("1" . " 50 in total")
               ("2" . " 30 in total")
               ("3" . "100 in total")
               ("4" . " 80 in total")))))
          (:div
           (#:reasons-to-reveal
            (radios
             "Suppose you face a decision with buttons to reveal reasons for each option. Then which of the following is true?"
             '(("1" . "You can choose an option and submit your choice only after revealing exactly one reason.")
               ("2" . "You can choose an option and submit your choice only after revealing both reasons.")
               ("3" . "You can choose an option and submit your choice after revealing any number of reasons.")))))
          submit-button))
        (lambda (#:when-paid when-paid
                 #:how-many-abstracts how-many-abstracts
                 #:how-many-baseline-abstracts n-baseline
                 #:reasons-to-reveal reasons-to-reveal)
          (define score
            (apply
             +
             (map
              (λ (b) (if b 1 0))
              (list
               (string=? when-paid "3")
               (string=? how-many-abstracts "4")
               (string=? n-baseline "2")
               (string=? reasons-to-reveal "1")))))
          (define last-attempt
            (hash 'when-paid (cons when-paid (string=? when-paid "3"))
                  'reasons-to-reveal (cons reasons-to-reveal (string=? reasons-to-reveal "1"))
                  'how-many-abstracts (cons how-many-abstracts (string=? how-many-abstracts "4"))
                  'how-many-baseline-abstracts (cons n-baseline (string=? n-baseline "2"))))
          (put 'all-attempts
               (cons last-attempt (get 'all-attempts null)))
          (put 'last-attempt last-attempt)
          (put 'attempt (add1 (get 'attempt)))
          (put 'comprehension-test-score score)))


       (pilot-instructions)

       ))))

  (define (introduction)
    (page
     (haml
      (.container

       (:h2 "Tutorial")

       (:p (format "This study consists of a brief (about ~a mins) tutorial session followed by a comprehension test. Then you can decide whether to continue in a follow-up study (the main session) or not. But to complete the Prolific study that you started, you do not have to participate in the main session, you only have to complete the tutorial and comprehension test, which serve to familiarize you with the main session, so you can decide whether you want to participate."
                   (conf 'pilot-tutorial-duration-estimate)))

       (button void "Continue")))))


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
                         (cond [(get 'pass-comprehension-test?)
                                (progress-update 'pass-comprehension-test? #t)
                                (progress-update 'completed-tutorial? #t)

                                done]

                               [else
                                (progress-update 'completed-tutorial? #t)
                                (progress-update 'pass-comprehension-test? #f)
                                (goto fail-comprehension-test)]))]
    [fail-comprehension-test --> fail-comprehension-test])

   (list
    (make-step 'introduction introduction)
    (make-step 'instructions
               (lambda ()
                 (page
                  (haml
                   (.container

                    (:p.info "Please take your time to read these instructions carefully.")

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
     (do-abstracts n-pilot-tutorial "banking" "tutorial" "Tutorial Tasks"
                   #:real-stakes? #f))
    (make-step 'display-correct-tutorial-answers
               (display-correct-answers '("tutorial") n-pilot-tutorial))
    (make-step/study
     'comprehension-test
     (comprehension-test-study pilot-comprehension-test 4)
     #:provide-bindings '([pass-comprehension-test? pass-test?]))
    (make-step 'fail-comprehension-test fail-comprehension-test))))



(define (consent-show-code)
  (page
   (haml
    (.container
     (:h1 "Enter Completion Code on Prolific")

     (:p.info "Since you agreed to participate in the main study, do not close this window.")

     (:p "Before continuing, enter the completion code on prolific. This entitles you to the baseline fee, the payments for the main study will be paid out as bonuses later on.")

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
      (:li (format "Baseline payment (for tutorial): $~a" (~r baseline-fee #:precision 2)))
      (:li (format "Bonus payment (for main session): $~a" (~r (+ completion-bonus abstract-bonus) #:precision 2))
           (:ul
            (:li (format "This consists of a $~a completion bonus and of a $~a bonus for correctly categorizing ~a abstracts."
                 completion-bonus
                 (~r #:precision 2 abstract-bonus)
                 score)))))))))



(define edpb-pilot
  (make-study
   "edpb pilot"
   #:transitions
   (transition-graph
    [landing-page --> assigning-roles --> ,route-participants]
    [error-page --> error-page]
    ;; Admin
    [admin --> admin]

    ;; Participant
    [waiting-page --> tutorial
                  --> consent
                  --> ,(lambda ()
                         (cond [(get 'consent-given?)
                                (progress-update 'consented? #t)
                                'consent-show-code ]

                               [else
                                (progress-update 'consented? #f)
                                'no-consent]))]

    [participant-took-study-already --> participant-took-study-already]
    [no-consent --> no-consent]
    [consent-show-code --> start-main-session-timer
                       --> main
                       --> debriefing
                       --> update-progress
                       --> payment-page]
    [payment-page --> payment-page])

   (list
    (make-step 'landing-page landing-page)
    (make-step 'assigning-roles assigning-roles)
    (make-step 'error-page error-page)
    (make-step/study 'tutorial (pilot-tutorial))
    (make-step 'consent consent)
    (make-step 'consent-show-code consent-show-code)
    (make-step/study 'admin admin-study)
    (make-step 'waiting-page waiting-page-pilot)
    (make-step 'participant-took-study-already participant-took-study-already)
    (make-step
     'start-main-session-timer
     (lambda ()
       (start-timer #:start-name 'main-session)
       (skip)))
    (make-step/study 'main pilot-main)
    (make-step 'debriefing debriefing)
    (make-step 'update-progress
               (lambda ()
                 (progress-update 'correct-abstract-tasks (get* 'abstract-task-score))
                 (progress-update 'total-abstracts-done (get* 'abstract-total-done))
                 (progress-update 'completed-main-session? #t)
                 (progress-update 'main-session-time
                                  (end-timer #:start-name 'main-session))
                 (skip)))
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
