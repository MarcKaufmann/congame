#lang racket/base

(require component
         (prefix-in forms: (only-in forms form))
         (except-in forms form)
         gregor
         marionette
         racket/contract
         racket/list
         koyo/haml
         koyo/job
         congame/components/bot
         congame/components/sentry
         congame/components/study
         congame/components/template
         congame-price-lists/price-lists
         congame/components/mail
         "study-tools.rkt"
         "tasks.rkt"
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 pjb-pilot-study)

(define (study-explanation)
  (define required-tasks (get 'required-tasks))
  (define practice-tasks (number->string (get 'practice-tasks)))
  (define participation-fee (get 'participation-fee))
  ((page/xexpr)
   (haml
   (:div.container.container
    (:h1 "Study Explanation")

    (:h2 "Tutorial")
    (:p "You are starting the tutorial for this study:")
    (:ul
     (:li "A study description (this page)")
     (:li "Describing the tasks in this study and doing " practice-tasks " practice tasks")
     (:li "Asking you if you agree to participate in the study"))
    (:p "You will not receive any payment for completing the tutorial.")

    (:h2 "Main Study")
    (:p "If you decide to participate in the study, you will do the following:")
    (:ul
     (:li "complete " required-tasks " required tasks")
     (:li "choose whether to do additional tasks for bonus payments")
     (:li "do another 10-minute task involving sound")
     (:li "fill in a brief survey"))
    (:p "If you complete the study, you will receive " (pp-money participation-fee) " as a participation fee, as well as any bonus payment from the additional tasks you choose.")

    (:p (:strong "Note:") " Since one of the tasks requires sound, you need to have headphones or be in a place where you can listen to sound." )

    (button void "Continue")))))

(define (consent)
  ((page/xexpr)
   (haml
   (:div.container
    (:h1 "Consent Form")
    (render-consent-form)))))

(define (test-comprehension)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Comprehension Tests")
     (render-comprehension-form)))))

(define (render-comprehension-form)
  (define the-form
    (form* ([understand? (ensure binding/text (required))])
           (list understand?)))
  ((page/xexpr)
   (haml
    (:div.container
     (form
      the-form
      ; after successful submit
      (λ (answer) (put 'comprehension-test answer))
      ; renderer: (-> rw xexpr)
      (λ (rw)
        `(div ((class "container"))
              (form ((action "")
                     (method "POST"))
                    (label
                     "Do you understand this?"
                     ,(rw "understand?" (widget-text)))
                    ,@(rw "understand?" (widget-errors))
                    (button ((type "Submit") (class "button")) "Submit")))))))))

(define (test-comprehension/bot)
  (define f (bot:find "form"))
  (for ([input (bot:element-find-all f "input")])
    (element-type! input "I, Robot"))
  (element-click! (bot:find "button[type=submit]")))

(define (render-requirements-form)
  (define the-form
    (form* ((play-audio? (ensure binding/boolean (required #:message "You cannot continue if you can't play the audio")))
            (has-audio? (ensure binding/boolean (required #:message "You cannot continue without audio")))
            (has-time?  (ensure binding/boolean (required #:message "You cannot continue if you don't have time"))))
           (hash 'play-audio? play-audio?
                 'has-audio? has-audio?
                 'has-time? has-time?
                 'satisfies-requirements? (and play-audio? has-audio? has-time?))))
  ((page/xexpr)
   (haml
    (form
     the-form
     (λ (answer)
       (put 'requirements-test answer)
       (put 'satisfies-requirements? (hash-ref answer 'satisfies-requirements?)))
     (λ (rw)
       `(div
         (div
          ,@(haml
             (:figure
              (:audio
               ([:src (resource-uri christmas-song)]))
              (.audio-controls
               (:button#playpause ((:type "button")) "Play/Pause")
               (:button#volume-up ((:type "button")) "Vol+")
               (:button#volume-down ((:type "button")) "Vol-"))
              (:figcaption "What a song"))
             (:script ([:src "js/audio-player.js"]))))
         (div
          (form ((action "")
                 (method "POST"))
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
                (button ((type "submit") (class "button")) "Submit")))))))))

(define (test-study-requirements-step/bot)
  (for ([checkbox (bot:find-all "input[type=checkbox]")])
    (displayln (format "checkbox is ~a" checkbox))
    (flush-output)
    (element-click! checkbox))
  (element-click! (bot:find "button[type=submit]")))

(define (test-study-requirements)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Requirements for Study")
     (:p "Please check the following requirements. If they do not hold, you cannot complete the study, hence you cannot continue:")
     (render-requirements-form)))))

(define (elicit-WTW)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Eliciting WTW")
     (button
      #:id 'willing
      (λ () (put 'WTW 5))
      "Willing to work")
     (button
      #:id 'not-willing
      (λ () (put 'WTW 0))
      "Not willing to work")))))

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

; Add treatment for length of break?
(define *rest-treatments* '(get-rest-then-elicit elicit-then-get-rest))
(define *task-treatments* '(1 3))

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
(define next-balanced-task-treatment (make-balanced-shuffle *task-treatments*))

(define (show-payments)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Payment Page")
     (:p "Within the next week, you will receive the following payments for your participation in this study:")
     (:p (get-total-payment))
     (:ul
      ,@(for/list ([(name payment) (in-hash (get-all-payments))])
          (haml
           (:li (symbol->string name) ": " (pp-money payment))))
      (button void "Finish Study"))))))

(define-job (send-study-completion-email p payment)
  (with-sentry
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

(define (render-debrief-form)
  (define the-form
    (form* ([gender (ensure binding/text (required))])
           gender))
  (define pid (current-participant-id))
  ((page/xexpr)
   (haml
    (form
     the-form
     (λ (survey-response)
       (put 'debrief-survey survey-response)
       ; FIXME: Where should I really call these functions?
       (put-payment! 'participation-fee (get 'participation-fee))
       (send-completion-email pid))
     (λ (rw)
       `(form ((action "")
               (method "POST"))
              (label
               "What is your gender?"
               ,(rw "gender" (widget-text)))
              ,@(rw "gender" (widget-errors))
              (button ((type "submit") (class "button")) "Submit")))))))

(define (debrief-survey/bot)
  (define f (bot:find "form"))
  (for ([input (bot:element-find-all f "input")])
    (element-type! input "Bot, James Bot"))
  (element-click! (bot:find "button[type=submit]")))

(define (debrief-survey)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Debrief Survey")
     (render-debrief-form)))))

(require (for-syntax racket/base) ; Needed to use strings in define-static-resource. Why? Just Cause.
         congame/components/resource)

;; Directory resources:
(define-static-resource songs "songs")

;; File resources:
(define-static-resource christmas-song (build-path "songs" "christmas.ogg"))

(define (get-rest)
  ((page/xexpr)
   (haml
    (:div.container
     ; FIXME: How can I ensure that the music is listened to at the normal pace before continuing is possible?
     ; Or at least that the continue button can only be clicked after a certain while? While JS solution might
     ; be good as a userfriendly interface, it should ultimately be enforced at the server level (I don't trust client side).
     (:h1 "Relax and listen to some music")
     (:figure
      (:audio
       ([:src (resource-uri christmas-song)]))
      (.audio-controls
       (:button#playpause "Play/Pause")
       (:button#volume-up "Vol+")
       (:button#volume-down "Vol-"))
      (:figcaption "What a song"))
     (:script ([:src "js/audio-player.js"]))
     (:br)
     (button void "Continue")))))

(define pl-extra-tasks
  (make-pl #:name 'pl1
           #:fixed-work 0
           #:fixed-money 0
           #:adjustable-work 10
           #:levels-of-money '(0 1 2 3)))

(define (determine-extra-tasks)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Determining extra tasks and payment you do now")
     (button
      (λ ()
        (define pl/answers (get 'WTW))
        (define pl/answers+choice (pl-random-choice pl/answers))
        (define extra-tasks (price-list-extra-work pl/answers+choice))
        (define extra-money (price-list-extra-money pl/answers+choice))
        (put 'WTW pl/answers+choice)
        (put 'extra-tasks extra-tasks)
        (put 'extra-money extra-money))
      "See extra tasks")))))

(define (see-extra-tasks)
  (define pl/answers+choice (get 'WTW))
  (define chosen (price-list-chosen pl/answers+choice))
  (define alternative (price-list-alternative pl/answers+choice))
  (define extra-tasks (get 'extra-tasks))
  (define continue-text
    (if (> extra-tasks 0)
        (format "Continue to ~a extra tasks" extra-tasks)
        "Continue with no extra tasks"))
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Extra Tasks")
     (:p "The choice that was randomly selected was between the following two options:")
     (:ol
      (:li (describe chosen))
      (:li (describe alternative)))
     (:p "You chose the first choice.")
     (button void continue-text)))))

(define elicit-WTW-and-work
  (make-study
   #:requires '()
   #:provides '(WTW)
   (list
    (make-step 'elicit-immediate-WTW
               ; TODO: Is it sensible to pass the pl-name as an argument? It is not a coherent mechanism.
               ; It feels to me that using steps is like using (limited) globals, while studies create
               ; their own scopes, making them more composable. Well, except some variables have to be shared.
               (price-list-step pl-extra-tasks #:pl-name 'WTW)
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
                       (when (get 'success?)
                         (put-payment! 'extra-tasks-bonus (get 'extra-money)))
                       done)
                     ; TODO: Can #:require-bindings take values, or does it have to refer to defined binding?
                     #:require-bindings '([n extra-tasks])
                     #:provide-bindings '([success? success?]))
    )))

(define (task-failure)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "You failed the tasks")
     (:p "You failed the tasks, therefore you cannot complete the study.")
     ; TODO: Improve how to deal with failures
     (button void "The end")
     ))))

(define (requirements-failure)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "You do not satisfy the requirements")
     (:p "You fail some of the requirements for the study, therefore you cannot complete the study.")
     (button void "The End")))))

(define (consent-failure)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "You did not consent")
     (:p "You did not consent to the study, therefore you cannot complete the study.")
     (button void "The End")))))

(define pjb-pilot-study-no-config
  (make-study
   #:requires '(participation-fee practice-tasks required-tasks)
   #:provides '(task-treatment rest-treatment)
   (list
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
           'task-failure
           'test-comprehension))
     ; TODO: Document that the LHS is the binding being assigned the value of the RHS
     #:require-bindings '([n practice-tasks])
     #:provide-bindings '([tutorial-success? success?]))
    (make-step 'test-comprehension test-comprehension #:for-bot test-comprehension/bot)
    (make-step
     'consent
     consent
     (λ ()
       ; TODO: The fact that I check only once means that, if by chance we jump past this stage
       ; then the study would simply continue. In general it might be good to have this property
       ; enforced from a given stage onwards.
       (cond [(not (get 'consent?))
              'consent-failure]
             [else
              ; TODO: Treatment assignment should also be done at the study, not step, level!!
              ; Can this be done, given the need for `put`?
              (put 'rest-treatment (next-balanced-rest-treatment))
              (put 'task-treatment (next-balanced-task-treatment))
              'required-tasks]))
     #:for-bot consent/bot)
    (make-step/study
     'required-tasks
     task-study
     (λ ()
       (if (not (get 'success?))
           'task-failure
           (case (get 'rest-treatment)
             [(get-rest-then-elicit) 'get-rest]
             [(elicit-then-get-rest) 'elicit-WTW-and-work])))
     #:require-bindings '([n task-treatment])
     #:provide-bindings '([success? success?]))
    (make-step 'get-rest
               get-rest
               #:for-bot bot:continuer
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
                     #:provide-bindings '([WTW WTW]))
    (make-step 'debrief-survey debrief-survey #:for-bot debrief-survey/bot)
    (make-step 'show-payments show-payments (λ () done) #:for-bot bot:continuer)
    (make-step 'task-failure task-failure (λ () done) #:for-bot bot:continuer)
    (make-step 'requirements-failure requirements-failure (λ () done) #:for-bot bot:continuer)
    (make-step 'consent-failure consent-failure (λ () done) #:for-bot bot:continuer)
    )))

; TODO: Allow `make-step/study` to refer to values, not just to symbols, so I don't need
; redundant wrapper steps like `welcome`.
(define (welcome)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Welcome")
     (:p "Start when you are ready.")
     (button
      (λ ()
        (put 'practice-tasks 3)
        (put 'participation-fee 2.00)
        (put 'required-tasks 15))
      "Start")))))

(define pjb-pilot-study
  (make-study
   #:requires '()
   #:provides '(task-treatment rest-treatment)
   (list
    (make-step 'welcome welcome)
    (make-step/study 'the-study
                     pjb-pilot-study-no-config
                     #:provide-bindings '([task-treatment task-treatment]
                                          [rest-treatment rest-treatment])
                     #:require-bindings '([practice-tasks practice-tasks]
                                          [participation-fee participation-fee]
                                          [required-tasks required-tasks])))))
