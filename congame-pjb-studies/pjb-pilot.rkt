#lang racket/base

(require component
         (prefix-in forms: (only-in forms form))
         (except-in forms form)
         (for-syntax racket/base) ; Needed to use strings in define-static-resource. Why? Just Cause.
         gregor
         koyo/haml
         koyo/job
         marionette
         racket/contract
         racket/list
         racket/match
         racket/random
         sentry
         (prefix-in config: congame/config)
         congame/components/bot
         congame/components/resource
         congame/components/study
         congame-pjb-studies/relax
         congame-price-lists/price-lists
         congame/tools
         "mail.rkt"
         "tasks.rkt"
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 pjb-pilot-study)

(define tutorial-fee 1.00)
(define required-matrix-piece-rate 0.10)
(define high-workload 15)
(define low-workload 10)
(define practice-tasks 2)
(define participation-fee 1.00)

;; TREATMENTS

(define *rest-treatments* '(get-rest-then-elicit elicit-then-get-rest))
(define *required-tasks-treatments* (list low-workload high-workload))

(define (make-balanced-shuffle original)
  (define ts (shuffle original))
  (λ ()
    (cond [(not (empty? ts))
           (begin0
               (first ts)
             (set! ts (rest ts)))]
          [else
           (set! ts (shuffle original))
           (first ts)])))

(define next-balanced-rest-treatment (make-balanced-shuffle *rest-treatments*))
(define next-balanced-required-tasks-treatment (make-balanced-shuffle *required-tasks-treatments*))

;;; PRICE-LIST CONFIGURATION

(define pl-length 15)
(define pl-stepsize 0.2)
(define money-levels
  (build-list pl-length (λ (i) (* i pl-stepsize))))
(define (pl-extra-tasks t name)
  (make-pl #:name name
           #:fixed-work 0
           #:fixed-money 0
           #:adjustable-work t
           #:levels-of-money money-levels))

(define (make-price-lists/tasks lot)
  (for/hash ([n lot])
    (define pl-name (string->symbol (string-append "pl" (number->string n))))
    (values pl-name (pl-extra-tasks n pl-name))))

(define PRICE-LISTS
  (make-price-lists/tasks '(5 8 11 15)))

;; PAGE RENDERERS

(define/contract (study-description required-tasks required-tasks-fee participation-fee)
  (-> number? number? number? any/c)
  (haml
   (:div
    (:h2 "Main Study Description")
    (:p "If you decide to participate in the study, you will do the following:")
    (:ul
     (:li "complete " (number->string required-tasks) " required tasks")
     (:li "choose whether to do extra tasks for bonus payments")
     (:li "do the extra tasks chosen (if applicable)")
     (:li "do a different 10-minute task involving sound/audio")
     (:li "fill in a brief survey"))

    (:h3 "Payments")
    (:p "You receive the following payments if you complete a given stage of the main study:")
    (:ul
     (:li (pp-money required-tasks-fee) " for required tasks")
     (:li (pp-money participation-fee) " if you complete the whole study. "
          (:strong "Note: ") "If you choose extra tasks, then you receive this bonus only if you complete the extra tasks. So if you do not want to do extra tasks, don't choose them.")
     (:li "If you choose and do the extra task, you will receive the corresponding bonus from that choice.")))))

(define (initialize)
  (define required-tasks (next-balanced-required-tasks-treatment))
  (put 'required-tasks required-tasks)
  (define required-tasks-fee
    (+ (* required-tasks required-matrix-piece-rate)
       ; high-required tasks treatment with 0 extra has same payment as low-required-tasks with extra payment
       ; To identify wealth effects
       (random-ref `(0.00 ,(* required-matrix-piece-rate
                              (- high-workload low-workload))))))
  (put 'required-tasks-fee required-tasks-fee)
  (skip))

(define (study-explanation)
  (define required-tasks (get 'required-tasks))
  (define practice-tasks (get 'practice-tasks))
  (define participation-fee (get 'participation-fee))
  (define required-tasks-fee (get 'required-tasks-fee))
  (page
   (haml
    (:div.container.container
     (:h1 "Study Instructions")

     (:h2 "Sound Required")
     (:p (:strong "Note:") " This study requires sound, so you need headphones or be able to listen to sound on your speakers." )

     (:h2 "Tutorial")
     (:p "You are starting the tutorial for this study which consists of the following:")
     (:ul
      (:li "a study description (this page)")
      (:li "a page to check that your sound works")
      (:li "a description of the tasks in this study followed by " (number->string practice-tasks) " practice tasks")
      (:li "a description of how your decisions determine optional extra tasks")
      (:li "a form asking whether you agree to participate in the study"))
     (:p "You will not receive any payment for completing the tutorial.")

     (study-description required-tasks required-tasks-fee participation-fee)


     (button void "Continue")))))

;;;; FORMS

(define (consent)
  (define required-tasks (get 'required-tasks))
  (define participation-fee (get 'participation-fee))
  (define required-tasks-fee (get 'required-tasks-fee))
  (page
   (haml
    (:div.container
     (render-consent-form)
     (.info
      (study-description required-tasks required-tasks-fee participation-fee))))))

;; Comprehension Formm

(define (test-comprehension)
  (page
   (haml
    (:div.container
     (:h1 "Comprehension Tests")
     (render-comprehension-form)
     (.info
      (study-description
       (get 'required-tasks)
       (get 'required-tasks-fee)
       (get 'participation-fee)))))))

(define ((is-equal a #:message [message #f]) v)
  (if (equal? v a)
      (ok v)
      (err (or message (format "Should be equal to ~a" a)))))

(define (render-comprehension-form)
  (define the-form
    (form* ([what-if-fail-study? (ensure
                                  binding/text
                                  (required)
                                  (is-equal "no-extra-no-participation-fee"
                                            #:message "No. You receive payment for required tasks, but not for the participation fee."))]
            [how-many-required-tasks? (ensure
                                       binding/number
                                       (required)
                                       (is-equal (get 'required-tasks)
                                                 #:message "No. Read the study description again."))])
           (list what-if-fail-study?
                 how-many-required-tasks?)))

  (haml
   (:div.container
    (form
     the-form
     ; after successful submit
     (λ (answer) (put 'comprehension-test answer))
     ; renderer: (-> rw xexpr)
     (λ (rw)
       (define n-tasks (number->string (get 'required-tasks)))
       `(div
         (p "The Study Instructions are repeated below.")
         (div ((class "group"))
              (label ((class "radio-group"))
                     "Suppose that after you complete the required tasks and make your choices, you end up with extra tasks. What happens if you fail the extra tasks -- either due to getting too many tasks wrong or not attempting them?"
                     ,(rw "what-if-fail-study?"
                          (widget-radio-group '(("no-payment-at-all" . "You will receive no payment at all")
                                                ("no-extra-bonus" . "You will not receive the extra bonus payment, but you will receive the participation fee and the payment for the required tasks")
                                                ("no-extra-no-participation-fee" . "You will receive the payment for the required tasks, but not the participation fee nor the extra bonus, since you cannot complete the study.")))))
              ,@(rw "what-if-fail-study?" (widget-errors)))
         (div ((class "group"))
              (label ((class "radio-group"))
                     "How many required tasks do you have to do?"
                     ,(rw "how-many-required-tasks?"
                          (widget-radio-group `(("0" . "0")
                                                ("6" . "6")
                                                (,n-tasks . ,n-tasks)
                                                ("13" . "13")))))
              ,@(rw "how-many-required-tasks?" (widget-errors)))
         (button ((type "Submit") (class "button")) "Submit")))))))

(define (test-comprehension/bot)
  (define f (bot:find "form"))
  (element-click! (bot:find "input[value='no-extra-no-participation-fee']"))
  ; FIXME: Brittle - relies on the correct answer being the third input.
  (element-click! (caddr (bot:find-all "input[name='how-many-required-tasks?']")))
  (element-click! (bot:find "button[type=submit]")))

;; Requirements Form

(define (render-requirements-form)
  (define the-form
    (form* ((play-audio? (ensure binding/boolean (required #:message "You cannot continue if you can't play the audio")))
            (has-audio? (ensure binding/boolean (required #:message "You cannot continue without audio")))
            (has-time?  (ensure binding/boolean (required #:message "You cannot continue if you don't have time"))))
           (hash 'play-audio? play-audio?
                 'has-audio? has-audio?
                 'has-time? has-time?
                 'satisfies-requirements? (and play-audio? has-audio? has-time?))))
  (haml
   (form
    the-form
    (λ (answer)
      (put 'requirements-test answer)
      (put 'satisfies-requirements? (hash-ref answer 'satisfies-requirements?)))
    (λ (rw)
      `(div
        (div
         ,(audio-container "test-audio.mp3" #:caption "Audio Test"))
        (label
         "Can you play and hear the above audio?"
         ,(rw "play-audio?" (widget-checkbox)))
        ,@(rw "play-audio?" (widget-errors))
        (br)
        (label
         "Can you listen to music/audio during this study?"
         ,(rw "has-audio?" (widget-checkbox)))
        ,@(rw "has-audio?" (widget-errors))
        (br)
        (label
         "Do you have time to finish the study within the next hour?"
         ,(rw "has-time?" (widget-checkbox)))
        ,@(rw "has-time?" (widget-errors))
        (br)
        (div ((class "hide-audio-button"))
             (button ((type "submit") (class "button")) "Submit")))))))

(define (test-study-requirements-step/bot)
  (for ([checkbox (bot:find-all "input[type=checkbox]")])
    (element-click! checkbox))
  (page-execute-async! (bot:current-page) "document.querySelector('button[type=submit]').click()"))

(define (test-study-requirements)
  (page
   (haml
    (:div.container
     (:h1 "Requirements for Study")
     (:p "Please check that you can play the test audio by hitting the play button, otherwise you cannot complete the study. Once the track has finished, a 'Continue' button will appear.")
     (render-requirements-form)))))

;; Debrief Form

(define ((input-in-range start end) v)
  (if (memq v (range start (add1 end)))
      (ok v)
      (err (format "You have to provide a number between ~a and ~a" start end))))

(define (render-debrief-form)
  (define the-form
    (form* ([gender (ensure binding/text (required))]
            [how-clear (ensure binding/number (required) (input-in-range 1 5))]
            [what-could-be-clearer (ensure binding/text)]
            [how-relaxing (ensure binding/number (required) (input-in-range 1 5))]
            [how-do-you-decide-on-extra-work (ensure binding/text (required))]
            [restful-activity (ensure binding/text (required))]
            [comments (ensure binding/text)]
            [work-factor-fewer-extra-tasks (ensure binding/boolean)]
            [work-factor-longer-break (ensure binding/boolean)]
            [work-factor-have-more-time-to-finish-study (ensure binding/boolean)]
            [work-factor-smaller-matrices (ensure binding/boolean)])
           (hash 'gender gender
                 'how-clear-were-instructions how-clear
                 'what-could-be-clearer what-could-be-clearer
                 'how-relaxing how-relaxing
                 'how-do-you-decide-on-extra-work how-do-you-decide-on-extra-work
                 'other-restful-activity restful-activity
                 'comments comments
                 'work-factors (hash 'fewer-extra-tasks work-factor-fewer-extra-tasks
                                     'longer-break work-factor-longer-break
                                     'have-more-time work-factor-have-more-time-to-finish-study
                                     'smaller-matrices work-factor-smaller-matrices))))
  (haml
   (form
    the-form
    (λ (survey-response)
      (put 'debrief-survey survey-response)
      (put-payment! 'participation-fee (get 'participation-fee)))
    (λ (rw)
      `(div
        (div ((class "group"))
             (label
              "What is your gender?"
              ,(rw "gender" (widget-text)))
             ,@(rw "gender" (widget-errors)))
        (div ((class "group"))
             (label
              "How clear were the instructions on a scale from 1 (very unclear) to 5 (very clear)?"
              ,(rw "how-clear" (widget-number)))
             ,@(rw "how-clear" (widget-errors)))
        (div ((class "group"))
             (label
              "If not, what could have been clearer?"
              ,(rw "what-could-be-clearer" (widget-text)))
             ,@(rw "what-could-be-clearer" (widget-errors)))
        (div ((class "group"))
             (label
              "How restful did you find the songs after the required tasks, from 1 (very un-relaxing) to 5 (very relaxing)?"
              ,(rw "how-relaxing" (widget-number)))
             ,@(rw "how-relaxing" (widget-errors)))
        (div ((class "group"))
             (label
              "What other activity would you find restful between two rounds of tasks? Think of activities you usually do during breaks from online work."
              ,(rw "restful-activity" (widget-text)))
             ,@(rw "restful-activity" (widget-errors)))
        (div ((class "group"))
             (label
              "Which of the following factors makes you more willing to do extra tasks -- i.e. you will accept them for less money?"
              (table
               (tr
                (td ,(rw "work-factor-fewer-extra-tasks" (widget-checkbox)) "Have fewer extra tasks to do")
                (td ,@(rw "work-factor-fewer-extra-tasks" (widget-errors))))
               (tr
                (td ,(rw "work-factor-longer-break" (widget-checkbox)) "Have a longer break")
                (td ,@(rw "work-factor-longer-break" (widget-errors))))
               (tr
                (td ,(rw "work-factor-have-more-time-to-finish-study" (widget-checkbox)) "Have more time to finish the study")
                (td ,@(rw "work-factor-have-more-time-to-finish-study" (widget-errors))))
               (tr
                (td ,(rw "work-factor-smaller-matrices" (widget-checkbox)) "Split each matrix into two smaller matrices")
                (td ,@(rw "work-factor-smaller-matrices" (widget-errors)))))))
        (div ((class "group"))
             (label
              "Based on what did you choose the level at which you were willing to do additional tasks?"
              ,(rw "how-do-you-decide-on-extra-work" (widget-text)))
             ,@(rw "how-do-you-decide-on-extra-work" (widget-errors)))
        (div ((class "group"))
             (label
              "If you have any other comments, write them here."
              ,(rw "comments" (widget-text)))
             ,@(rw "comments" (widget-errors)))
        (button ((type "submit") (class "button next-button")) "Submit"))))))

(define (debrief-survey/bot)
  (define f (bot:find "form"))
  (for ([input (bot:element-find-all f "input[type=text]")])
    (element-type! input "Bot, James Bot"))
  (for ([input (bot:element-find-all f "input[type=number]")])
    (element-type! input "5"))
  (element-click! (bot:find "button[type=submit]")))

(define (debrief-survey)
  (page
   (haml
    (:div.container
     (:h1 "Debrief Survey")
     (render-debrief-form)))))

;;;;;; HANDLERS

;; TODO: cleanup.
(provide
 willing-to-work?)

;; TODO: Parameterized bots need bespoke values to try to ensure that
;; bot writers provide the right arguments.
(struct bot-willing-to-work? (yes?)
  #:transparent)

(define/contract (willing-to-work? willing?)
  (-> boolean? bot-willing-to-work??)
  (bot-willing-to-work? willing?))

(define/contract (elicit-WTW/bot bwt)
  (-> bot-willing-to-work? void?)
  (bot:click (if (bot-willing-to-work?-yes? bwt)
                 'willing
                 'not-willing)))

(define (show-payments)
  (define (payment-display-name n)
    (case n
      [(tutorial-fee) "Completing the tutorial"]
      [(participation-fee) "Completing the study (participation fee)"]
      [(required-tasks-fee) "Doing the required tasks"]
      [(extra-tasks-bonus) "Bonus for extra tasks"]
      [else n]))
  (page
   (haml
    (:div.container
     (:h1 "Payment Page")
     (:p "Within the next week, you will receive a total payment (baseline plus bonuses) of " (get-total-payment) " for this study. The detailed breakdown is as follows:")
     (:ul
      ,@(for/list ([(name payment) (in-hash (get-all-payments))])
          (haml
           (:li (payment-display-name name) ": " (pp-money payment)))))
     (:p "Shortly after finishing the study, you will receive an email from us. " (:a ((:href (string-append "mailto:" config:support-email))) "Email us") " if you have not received the payment by the end of next week." )
     (button
      (λ ()
        (send-completion-email (current-participant-id)))
      "Finish Study")))))

(define-job (send-study-completion-email p payment)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (sentry-capture-exception! e)
                     (raise e))])
    (define mailer (system-ref 'mailer))
    (mailer-send-study-completed-email mailer p payment)))

(define (get-total-payment)
  (pp-money
   (apply + (hash-values (get-all-payments)))))

(define (send-completion-email pid)
  (unless (current-user-bot?)
    (schedule-at
     (now/moment)
     (send-study-completion-email
      (participant-email pid)
      (get-total-payment)))))

(define (determine-extra-tasks)
  (page
   (haml
    (:div.container
     (:h1 "Determining the choice that counts")
     (:p "The computer will now randomly determine one of the choice pages, and one of the choices on that page as the choice that counts.")
     (button
      (λ ()
        (define pls (get 'price-lists))
        ; FIXME: I still rely on price-list names being unique,
        ; and not clashing with other key-names.
        (define wtws
          (for/hash ([pl pls])
            (values pl (price-list-switch (get pl)))))
        (put 'WTWs wtws)
        ; pl-that-counts is the name of the price-list that counts
        (define pl-that-counts (random-ref pls))
        (put 'price-list-that-counts pl-that-counts)
        (define pl/answers (get pl-that-counts))
        (define pl/answers+choice (pl-random-choice pl/answers))
        ; Store price list with the choice that counts. TODO: Dangerous to overwrite original, no?
        (put 'choice-that-counts pl/answers+choice)
        (match-define (option extra-tasks extra-bonus)
          (price-list-chosen pl/answers+choice))
        (put 'extra-tasks extra-tasks)
        (put 'extra-bonus extra-bonus))
      "See extra tasks")))))

(define (see-extra-tasks)
  (define pl/answers+choice (get 'choice-that-counts))
  (define chosen (price-list-chosen pl/answers+choice))
  (define alternative (price-list-alternative pl/answers+choice))
  (define extra-tasks (get 'extra-tasks))
  (define extra-bonus (pp-money (get 'extra-bonus)))
  (define continue-text
    (if (> extra-tasks 0)
        (format "Continue to ~a extra tasks for a ~a extra bonus " extra-tasks extra-bonus)
        "Continue with no extra tasks and no extra bonus."))
  (page
   (haml
    (:div.container
     (:h1 "Extra Tasks")
     (:p "The choice that was randomly selected was between the following two options, from which you chose the one listed first:")
     (:ol
      (:li (describe chosen))
      (:li (describe alternative)))
     (:p continue-text)
     (button void continue-text)))))

(define (introduce-WTW)
  (define pls (get 'price-lists))
  (define n (length pls))
  (page
   (haml
    (.container
     (:h1 "Choices for Extra Tasks")
     (:ol
      (:li "On the next " (number->string n) " pages, you will make choices about doing extra tasks for bonus payments")
      (:li "Then the computer randomly picks one of the pages as the page-that-counts, and one of the choices on that page as the choice-that-counts")
      (:li "You will then be asked to do the extra tasks you chose for the choice-that-counts, which may be 0")
      (:li "Thus every choice may become the choice-that-counts"))
     (:p (:strong "Remember: ") "If you choose extra tasks, but fail to do them, you forfeit both the extra bonus and the participation bonus.")
     (button
      (λ ()
        (put 'remaining-price-lists (shuffle pls))
        (put 'answered-price-lists '()))
      "Continue")))))

(define (task-failure)
  (page
   (haml
    (:div.container
     (:h1 "You failed the tasks")
     (:p "You failed the tasks and cannot continue the study.")
     ; TODO: Improve how to deal with failures
     #;(button void "The end")
     ))))

(define-static-resource price-list-screenshot "price-list-screenshot.png")

(define (tutorial-illustrate-elicitation)
  (page
   (haml
    (:div.container
     (:h1 "Explaining Choices for Extra Tasks")
     (:p "You will be given several choice pages for doing extra tasks. Below is a screenshot of one choice page with some choices made. Your choices determine the extra tasks and extra bonus in the study as follows:")
     (:ol
      (:li "The computer randomly selects one of the choice pages as the page-that-counts")
      (:li "The computer randomly selects one of the choices on that page as the choice-that-counts")
      (:li "If you chose the option without extra tasks, then you do not have to do any additional tasks and receive no extra bonus")
      (:li "If you chose the option with extra tasks, then you have to do that many extra tasks and receive the extra bonus for that option. "))
     (:p (:strong "Note:") " You cannot skip the extra tasks: you can only complete the study and receive the participation bonus if you do the extra tasks!")
     (.container.screenshot
      (:h2 "Screenshot of an example Decision Page")
      (:p "Suppose that the computer randomly selected the 8th choice on this page as the choice-that-counts. Then: ")
      (:ul
       (:li "The person would have to do 7 extra tasks and receive an extra bonus of $1.40 in addition to their other payments")
       (:li "If the person fails to do the 7 extra tasks, they receive neither the completion fee, nor the extra bonus of $1.40 -- only payments for parts of the study they have already completed"))
      (:img ([:src (resource-uri price-list-screenshot)])))
     (button void "Continue")))))

(define elicit-WTW-and-work
  (make-study
   "elicit-WTW-and-work"
   #:requires '(price-lists)
   #:provides '(WTWs)
   (list
    (make-step 'introduce-WTW introduce-WTW)
    (make-step 'elicit-immediate-WTW
               (λ ()
                 (define next-pl-name
                   (car (get 'remaining-price-lists)))
                 (define n (add1 (length (get 'answered-price-lists))))
                 ((price-list-step (hash-ref PRICE-LISTS next-pl-name)
                                  #:title (format "Extra Work Choice Number ~a" n)
                                  #:pl-name next-pl-name)))
               (λ ()
                 (define pls (get 'remaining-price-lists))
                 (define previously-answered-pls
                   (get 'answered-price-lists))
                 (put 'answered-price-lists (cons (car pls) previously-answered-pls))
                 (define remaining-pls (cdr pls))
                 (put 'remaining-price-lists remaining-pls)
                 ; TODO: Should I reverse the answered price-lists so that I get original order?
                 (if (empty? remaining-pls)
                     'determine-extra-tasks
                     'elicit-immediate-WTW))
               #:for-bot price-list-step/bot)
    (make-step 'determine-extra-tasks
               determine-extra-tasks)
    (make-step 'see-extra-tasks
               see-extra-tasks
               (λ ()
                 (if (> (get 'extra-tasks) 0)
                     'extra-tasks
                     done))
               #:for-bot bot:continuer)
    (make-step/study 'extra-tasks
                     task-study
                     (λ ()
                       (cond
                         [(get 'success?)
                          (put-payment! 'extra-tasks-bonus (get 'extra-bonus))
                          done]
                         [else
                          'fail]))
                     #:require-bindings '([n extra-tasks]
                                          ; The value was passed in even when it wasn't yet required! BUG?
                                          [max-wrong-tasks extra-tasks]
                                          [title (const "Extra Tasks and Bonus")]
                                          [hide-description? (const #t)])
                     #:provide-bindings '([success? success?]))
    (make-step 'fail task-failure))))

(define (requirements-failure)
  (page
   (haml
    (:div.container
     (:h1 "You do not satisfy the requirements")
     (:p "You fail some of the requirements for the study, therefore you cannot complete the study.")
     (button void "The End")))))

(define (consent-failure)
  (page
   (haml
    (:div.container
     (:h1 "You did not agree to participate")
     (:p "You did not consent to the study, therefore you will not continue to the study. We will now show you the payments and then provide you with the completion code for the tutorial.")
     (button void "Continue to Payments")))))

;;; MAIN STUDY

(define pjb-pilot-study-no-config
  (make-study
   "pjb-pilot-study-no-config"
   #:requires '(participation-fee
                practice-tasks
                price-lists
                tutorial-fee)
   #:provides '(rest-treatment)
   (list
    (make-step 'initialize initialize)
    (make-step 'explain-study study-explanation)
    (make-step
     'test-study-requirements
     test-study-requirements
     (λ ()
       (if (not (get 'satisfies-requirements?))
           'requirements-failure
           'tutorial-tasks))
     #:for-bot test-study-requirements-step/bot)
    (make-step/study
     'tutorial-tasks
     task-study
     (λ ()
       (if (not (get 'tutorial-success?))
           (fail 'fail-tutorial-tasks)
           'tutorial-illustrate-elicitation))
     #:require-bindings '([n practice-tasks]
                          [max-wrong-tasks practice-tasks]
                          [title (const "Practice Tasks")]
                          [hide-description? (const #f)])
     #:provide-bindings '([tutorial-success? success?]))
    (make-step 'tutorial-illustrate-elicitation tutorial-illustrate-elicitation)
    (make-step 'test-comprehension test-comprehension #:for-bot test-comprehension/bot)
    (make-step
     'consent
     consent
     (λ ()
       ; TODO: The fact that I check only once means that, if by chance we jump past this stage
       ; then the study would simply continue. In general it might be good to have this property
       ; enforced from a given stage onwards.
       (put-payment! 'tutorial-fee (get 'tutorial-fee))
       (cond [(not (get 'consent?))
              (fail 'fail-no-consent)]
             [else
              ; TODO: Treatment assignment should also be done at the study, not step, level!!
              ; Can this be done, given the need for `put`?
              (put 'rest-treatment (next-balanced-rest-treatment))
              'required-tasks]))
     #:for-bot consent/bot)
    (make-step/study
     'required-tasks
     task-study
     (λ ()
       (cond [(not (get 'success?))
              (fail 'fail-required-tasks)]
             [else
              (put-payment! 'required-tasks-fee (get 'required-tasks-fee))
              (case (get 'rest-treatment)
                [(get-rest-then-elicit) 'get-rest]
                [(elicit-then-get-rest) 'elicit-WTW-and-work])]))
     #:require-bindings '([n required-tasks]
                          [max-wrong-tasks required-tasks]
                          [title (const "Required Tasks")]
                          [hide-description? (const #t)])
     #:provide-bindings '([success? success?]))
    (make-step/study 'get-rest
                     (relax-study)
                     (λ ()
                       (case (get 'rest-treatment)
                         [(get-rest-then-elicit) 'elicit-WTW-and-work]
                         [(elicit-then-get-rest) 'debrief-survey])))
    (make-step/study 'elicit-WTW-and-work
                     elicit-WTW-and-work
                     (λ ()
                       (case (get 'rest-treatment)
                         [(get-rest-then-elicit) 'debrief-survey]
                         [(elicit-then-get-rest) 'get-rest]))
                     #:require-bindings '([price-lists price-lists])
                     #:provide-bindings '([WTWs WTWs]))
    (make-step 'debrief-survey debrief-survey (λ () done) #:for-bot debrief-survey/bot)
    (make-step 'requirements-failure requirements-failure (λ () done) #:for-bot bot:continuer))))

(define pjb-pilot-study
  (make-study
   "pjb-pilot-study"
   #:requires '()
   #:provides '(rest-treatment)
   #:failure-handler (lambda (s reason)
                       (put 'fail-status reason)
                       (eprintf "failed at ~e with reason ~e~n" s reason)
                       (put 'rest-treatment 'fail)
                       reason)
   (list
    (make-step/study 'the-study
                     pjb-pilot-study-no-config
                     (lambda () 'show-payments)
                     #:provide-bindings '([rest-treatment rest-treatment])
                     #:require-bindings `([practice-tasks (const ,practice-tasks)]
                                          [participation-fee (const ,participation-fee)]
                                          [tutorial-fee (const ,tutorial-fee)]
                                          [price-lists (const ,(hash-keys PRICE-LISTS))]))
    (make-step 'fail-no-consent consent-failure (λ () 'show-payments) #:for-bot bot:continuer)
    (make-step 'fail-tutorial-tasks
               (lambda ()
                 (page
                  (haml
                   (.container
                    (:h1 "You failed the tasks")
                    (:p "The study ends here, since you failed too many tasks.")
                    (button void "Finish Study")))))
               (λ () done))
    (make-step 'fail-required-tasks
               (lambda ()
                 (page
                  (haml
                   (.container
                    (:h1 "You failed the tasks")
                    (:p "The study ends here, since you failed too many tasks.")
                    (button void "See payments")))))
               (λ () 'show-payments))
    (make-step 'show-payments show-payments (λ () done) #:for-bot bot:continuer))))
