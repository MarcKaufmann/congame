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
; - instruction screen
; - add example choice to tutorial, including choice that counts, and display a choice with a reason, but without allowing to reveal a reason (IF we decide to already explain it in the tutorial).
; - choice screens:
;   - display reasons
;   - create all the choices with all the reasons that we have in mind right now
; - display the choice that counts
; - compute total payment
; - Add comprehension question about the reasons

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



(define (no-consent)
  (page
   (haml
    (.container
     (:h1 "Thanks for considering the study")

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

(define (comprehension-test-success?)
  (> (get 'comprehension-test-score) 2))

(define (tutorial)
  (make-study
   "edpb-tutorial"
   #:transitions
   (transition-graph
    [instructions --> initialize
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
    (make-step 'instructions instructions)
    (make-step 'initialize
               (lambda ()
                 (put 'tutorial-example/in (car (random-abstracts/topic 1 "sports")))
                 (put 'tutorial-example/out (car (random-abstracts/non-topic 1 "sports")))
                 (skip)))
    (make-step
     'task-description
     (lambda ()
       (abstract-examples
        (get 'tutorial-example/in)
        (get 'tutorial-example/out))))
    (make-step 'comprehension-test comprehension-test)
    (make-step/study
     'tutorial-tasks
     (lambda ()
       ((do-abstracts 2 "gender inequality" tutorial))))
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
     ; FIXME: update to have the type.
     (format "Categorize ~a abstracts ~a into '~a' or 'Other'."
             amount
             (if (equal? session 'session1) "today" "next session")
             (cond [(string=? (car type) "ai") "AI"]
                   [else
                    (string-titlecase (car type))]))]))

(define/contract (abstract-choice ce k)
  ; FIXME: does page return an xexpr?
  (-> choice-env? symbol? any/c)
  (define (put/choice o)
    (define choices
      (get k '()))
    (put k (cons (cons (string->symbol o) ce) choices)))
  (define (ce-reason ce k)
    (case k
      [(A) (o+r-reason (choice-env-A ce))]
      [(B) (o+r-reason (choice-env-B ce))]))
  (define reasons?
    (or (ce-reason ce 'A)
        (ce-reason ce 'B)))
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (:div
         (#:choice
            (make-radios
             `((A . (,(describe-abstracts ce 'A) ,(ce-reason ce 'A)))
               (B . (,(describe-abstracts ce 'B) ,(ce-reason ce 'B))))
             (lambda (options make-radio)
               (haml
                (:div
                 (:h3 "Description of Options")
                 ,@(for/list ([pair (in-list options)])
                     (define l (car pair))
                     (define label (cadr pair))
                     (define r (caddr pair))
                     (haml
                      (:div
                       (:h4 (format "Option ~a" l))
                       (:p label)

                       (when (ce-reason ce l)
                         (haml
                          (:table
                           (:tbody
                            (:tr
                             (:td
                              (:a.button.button--reason
                               ([:onclick (format "revealReason('~a');" l)]) "Reveal reason " (:strong (~a (reason-dir r))) " Option " (symbol->string l)))
                             (:td
                              (case l
                                [(A) (haml
                                      (.reason#A ([:style "display: none;"]) (reason-text r)))]
                                [(B) (haml
                                      (.reason#B ([:style "display: none;"]) (reason-text r)))]))))))))))

                  (:h3 "Choose an Option")
                   (:table
                    (:tbody
                     ,@(for/list ([l (list 'A 'B)])
                         (haml
                           (:tr
                            (:td (make-radio l))
                            (:td (format "Option ~a" l))))))))))))

         (if reasons?
             (haml
              (:button.button.next-button#submit ([:type "submit"] [:disabled "true"]) "Submit (disabled until you reveal a reason)"))
             (haml
              (:button.button.next-button#submit ([:type "submit"]) "Submit"))))
        (:script
         #<<SCRIPT
var reveals = 0;
function revealReason(label) {
    if (reveals == 0) {
        reveals = 1;
        document.getElementById(label).style.display = 'block';
        submit = document.getElementById('submit');
        submit.disabled = false;
        submit.textContent = 'Submit';
    }
}
SCRIPT
         )))
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
   #:provides '(work-choices)
   (list
    (make-step 'choice-page (lambda ()
                              (define next-ce
                                (car (get 'remaining-choices)))
                              (abstract-choice next-ce 'work-choices))))))

(define (set-treatments)
  ; FIXME: Needs to be determined once work choices etc display properly
  (put 'choices-to-make
       (list

        (choice-env
         (o+r (option 'session1 '("socioeconomic inequality" "other") 5) (reason 'for "'tis good"))
         (o+r (option 'session1 '("sports" "other") 5) (reason 'against "dis BAD!")))

        (choice-env
         (o+r (option 'session1 '("socioeconomic inequality" "other") 5) #f)
         (o+r (option 'session1 '("sports" "other") 5) #f))

        (choice-env
         (o+r (option 'session2 '("sports" "other") 10) (reason 'for "Y not!"))
         (o+r (option 'session1 '("sports" "other") 15) #f))))
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

(define (set-pilot-choices)
  ; FIXME: add all the choices that I want to be made
  ; FIXME: put all the randomization of orders etc here
  (put 'choices-to-make
       (list

        (choice-env
         (o+r (option 'session1 '("socioeconomic inequality" "other") 5) (reason 'for "'tis good"))
         (o+r (option 'session1 '("gender inequality" "other") 5) (reason 'against "dis BAD!")))

        (choice-env
         (o+r (option 'session1 '("socioeconomic inequality" "other") 5) #f)
         (o+r (option 'session1 '("gender inequality" "other") 5) #f))

        (choice-env
         (o+r (option 'session2 '("sports" "other") 15) (reason 'for "Y not!"))
         (o+r (option 'session1 '("sports" "other") 10) #f))))
  (skip))

(define (determine-pilot-choices)
  ; FIXME: This is some ugly and tedious code.
  (define cs (get 'choices-made))
  (define c (random-ref cs))
  (define A-or-B (car c))
  (define ce (cdr c))
  ;(eprintf "Choice environment that counts: ~a" ce)
  (match-define (choice-env (o+r A RA)
                            (o+r B RB))
    ce)
  (define-values (o r)
    (if (equal? A-or-B 'A)
        (values A RA)
        (values B RB)))
  (eprintf "Option that counts: ~a" o)
  (put 'additional-option o)
  (put 'additional-reason r)
  (match-define (option session (list category non-category) n)
    o)
  (put 'additional-n n)
  (put 'additional-category (string-downcase category))
  (put 'additional-non-category (string-downcase non-category))
  (page
   (haml
    (.container
     (:h1 "Determined choice that counts")

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

(define ((do-abstracts n category prefix))
  (define (~prefix s)
    (string->symbol (string-append prefix "-" s)))
  (define abstracts
    (sample-work-abstracts category n))
  (put (~prefix "n") n)
  (put (~prefix "category") category)
  (put (~prefix "category") abstracts)
  (define total (length abstracts))
  (for/study ([(abs-task i) (in-indexed (in-list abstracts))])
    (put-current-round-name (format "abstract ~s" i))
    (abstract-task/page abs-task i total category)))

(define pilot-main
  (make-study
   "main part of pilot"
   #:transitions
   (transition-graph
    [set-choices --> choices
                 --> determine-choice-that-counts
                 --> do-baseline-work
                 --> do-additional-work
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
    (make-step/study
     'do-baseline-work
     (do-abstracts 2 "gender inequality" "baseline"))

    (make-step/study
     'do-additional-work
     (lambda ()
       ((do-abstracts
        (get 'additional-n)
        (get 'additional-category)
        "additional-work")))))))

(define (pilot-tutorial)

  (define (pilot-comprehension-test)
    (page
     (haml
      (.container
       (:h1 "Comprehension Test")

       (button void "Next")))))

  (make-study
   "pilot tutorial"
   #:transitions
   (transition-graph
    [instructions --> initialize
                  --> task-description
                  --> tutorial-tasks
                  --> comprehension-test
                  --> ,(lambda () next)])
   (list
    (make-step 'instructions pilot-instructions)
    (make-step 'initialize
               (lambda ()
                 (put 'tutorial-example/in (car (random-abstracts/topic 1 "sports")))
                 (put 'tutorial-example/out (car (random-abstracts/non-topic 1 "sports")))
                 (skip)))
    (make-step
     'task-description
     (lambda ()
       (abstract-examples
        (get 'tutorial-example/in)
        (get 'tutorial-example/out))))
    (make-step 'tutorial-tasks (make-stub "Tutorial Tasks"))
    (make-step 'comprehension-test pilot-comprehension-test))))

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
                         (if (get 'consent-given?) 'main 'no-consent))]

    [no-consent --> no-consent]
    [main --> debriefing]
    [debriefing --> debriefing])

   (list
    (make-step 'assigning-roles assigning-roles)
    (make-step 'error-page error-page)
    (make-step/study 'tutorial (pilot-tutorial))
    (make-step 'consent consent)
    (make-step/study 'admin admin-study)
    (make-step 'waiting-page waiting-page)
    (make-step/study 'main pilot-main)
    (make-step 'debriefing (make-stub "Debriefing"))
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
