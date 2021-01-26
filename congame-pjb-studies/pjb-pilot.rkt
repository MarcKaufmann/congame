#lang racket/base

(require (prefix-in forms: (only-in forms form))
         (except-in forms form)
         marionette
         racket/contract
         racket/list
         koyo/haml
         congame/components/study
         congame-price-lists/price-lists
         "study-tools.rkt"
         "tasks.rkt"
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 pjb-pilot-study)

(define (study-explanation)
  (put 'practice-tasks 3)
  (put 'participation-fee 2.00)
  (put 'required-tasks 15)
  (define required-tasks (get 'required-tasks))
  (define practice-tasks (number->string (get 'practice-tasks)))
  (define participation-fee (get 'participation-fee))
  (haml
   (:div.container
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

    (button void "Continue"))))

(define (consent)
  (haml
   (:div
    (:h1 "Consent Form")
    (render-consent-form))))

(define (test-comprehension)
  (haml
   (:div
    (:h1 "Comprehension Tests")
    (render-comprehension-form))))

(define (render-comprehension-form)
  (define the-form
    (form* ([understand? (ensure binding/text (required))])
           (list understand?)))
  (haml
    (form
     the-form
     ; after successful submit
     (λ (answer) (put 'comprehension-test answer))
     ; renderer: (-> rw xexpr)
     (λ (rw)
       `(form ((action "")
               (method "POST"))
              (label
               "Do you understand this?"
               ,(rw "understand?" (widget-text)))
              ,@(rw "understand?" (widget-errors))
              (button ((type "Submit")) "Submit"))))))

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
  (haml
   (form
    the-form
    (λ (answer)
      (put 'requirements-test answer)
      (put 'satisfies-requirements? (hash-ref answer 'satisfies-requirements?)))
    (λ (rw)
      `(div
        (div
         (audio
          ((controls "")
           (src ,(resource-uri christmas-song)))))
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
               (button ((type "submit")) "Submit"))))))))

(define (test-study-requirements-step/bot)
  (for ([checkbox (bot:find-all "input[type=checkbox]")])
    (displayln (format "checkbox is ~a" checkbox))
    (flush-output)
    (element-click! checkbox))
  (element-click! (bot:find "button[type=submit]")))

(define (test-study-requirements)
  (haml
   (:div
    (:h1 "Requirements for Study")
    (:p "Please check the following requirements. If they do not hold, you cannot complete the study, hence you cannot continue:")
    (render-requirements-form))))

(define (elicit-WTW)
  (haml
   (:div
    (:h1 "Eliciting WTW")
    (button
     #:id 'willing
     (λ () (put 'WTW 5))
     "Willing to work")
    (button
     #:id 'not-willing
     (λ () (put 'WTW 0))
     "Not willing to work"))))

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

; TODO: `make-balanced-shuffle` should be provided by a module of helper functions
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

; FIXME: This setup does not balance at the rest x task treatments jointly
(define next-balanced-rest-treatment (make-balanced-shuffle *rest-treatments*))
(define next-balanced-task-treatment (make-balanced-shuffle *task-treatments*))


(define (show-payments)
  ; TODO: implement
  (haml
   (:div
    (:h1 "Payment Page")
    (:p "Within the next week, you will receive the following payments for your participation in this study:")
    (:p "TBD")
    (button void "Finish Study"))))

(define (compute-payments)
  (void))

(define (send-completion-email)
  ; TODO: implement
  void)

(define (render-debrief-form)
  (define the-form
    (form* ([gender (ensure binding/text (required))])
           gender))
  (haml
   (form
    the-form
    (λ (survey-response)
      (put 'debrief-survey survey-response)
      (compute-payments)
      (send-completion-email))
    (λ (rw)
      `(form ((action "")
              (method "POST"))
             (label
              "What is your gender?"
              ,(rw "gender" (widget-text)))
             ,@(rw "gender" (widget-errors))
             (button ((type "submit")) "Submit"))))))

(define (debrief-survey/bot)
  (define f (bot:find "form"))
  (for ([input (bot:element-find-all f "input")])
    (element-type! input "Bot, James Bot"))
  (element-click! (bot:find "button[type=submit]")))

(define (debrief-survey)
  (haml
   (:div
    (:h1 "Debrief Survey")
    (render-debrief-form))))

(require (for-syntax racket/base) ; Needed to use strings in define-static-resource. Why? Just Cause.
         congame/components/resource)

;; Directory resources:
(define-static-resource songs "songs")

;; File resources:
(define-static-resource christmas-song (build-path "songs" "christmas.ogg"))

(define (get-rest)
  (haml
   (:div
    ; TODO: How can I ensure that the music is listened to at the normal pace before continuing is possible?
    (:h1 "Relax and listen to some music")
    (:audio
     ([:controls ""]
      [:src (resource-uri christmas-song)]))
    (:br)
    (button void "Continue"))))

(define pl-extra-tasks
  (make-pl #:name 'pl1
           #:fixed-work 0
           #:fixed-money 0
           #:adjustable-work 10
           #:levels-of-money '(0 1 2 3)))

; TODO: Would it make sense to define transitions separately from steps, so that we have a list of
; steps available at a given study-level (including steps for substudies) and a separate part for
; the logic of the study in terms of transitions, which ignores handlers and bots, and only
; relates conditions for how steps follow each other based on available variables and treatments.
; It may even be better to define transitions at the study, rather than at the step, level. Then
; a situation where the value of a treatment leads to A -> B -> C, while another leads to C -> B -> A
; can easily be defined at the study level, whereas it requires repeatedly defining and checking things
; for each step: each step needs to check the value of the treatment, so the code has to be defined
; again and again, even though it really just needs to be written once at the study level.
; Probably it is best to keep the ability of steps overriding the study-transition, but adding
; functionality for defining step-transition at the study-level.
(define (determine-extra-tasks)
  ; TODO: Needs to do the appropriate choice from the WTW answer
  (haml
   (:div
    (:h1 "Determining extra tasks and payment you do now")
    (button
     (λ ()
       (define pl/answers (get 'WTW))
       (define pl/answers+choice (pl-random-choice pl/answers))
       (define extra-tasks (price-list-extra-work pl/answers+choice))
       (put 'WTW pl/answers+choice)
       (put 'extra-tasks extra-tasks))
     "See extra tasks"))))

(define (see-extra-tasks)
  (define pl/answers+choice (get 'WTW))
  (define chosen (price-list-chosen pl/answers+choice))
  (define alternative (price-list-alternative pl/answers+choice))
  (define extra-tasks (get 'extra-tasks))
  (define continue-text
    (if (> extra-tasks 0)
        (format "Continue to ~a extra tasks" extra-tasks)
        "Continue with no extra tasks"))
  (haml
   (:div
    (:h1 "Extra Tasks")
    (:p "The choice that was randomly selected was between the following two options:")
    (:ol
     (:li (describe chosen))
     (:li (describe alternative)))
    (:p "You chose the first choice.")
    (button void continue-text))))

(define elicit-WTW-and-work
  (make-study
   #:requires '()
   #:provides '(WTW)
   (list
    (make-step 'elicit-immediate-WTW
               ; TODO: Is it sensible to pass the put-name as an argument? It is not a coherent mechanism.
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
                     (λ () done)
                     ; TODO: Can #:require-bindings take values, or does it have to refer to defined binding?
                     #:require-bindings '([n extra-tasks])
                     #:provide-bindings '([success? success?])))))

(define (task-failure)
  (haml
   (:div
    (:h1 "You failed the tasks")
    (:p "You failed the tasks, therefore you cannot complete the study.")
    ; TODO: Is there a reason why we should have a button here to continue anyway? E.g. in terms
    ; of marking the study as done? Ideally states such as this wouldn't require a further click.
    (button void "The end")
    )))

(define (requirements-failure)
  (haml
   (:div
    (:h1 "You do not satisfy the requirements")
    (:p "You fail some of the requirements for the study, therefore you cannot complete the study.")
    (button void "The End"))))

(define (consent-failure)
  (haml
   (:div
    (:h1 "You did not consent")
    (:p "You did not consent to the study, therefore you cannot complete the study.")
    (button void "The End"))))

(define pjb-pilot-study
  (make-study
   ; FIXME: No natural way to pass in #:requires for top-level study. The requires should
   ; provided when creating a study-instance as parameters to configure.
   #:requires '()
   ; FIXME: #:provides should include WTW, but we won't get that if the person fails the tasks.
   ; Upon failing a study, call/set some default values for provide, error codes attached to data or NA.
   #:provides '(task-treatment rest-treatment)
   ;; TODO: Ensure work is done in appropriate time, i.e. all in one go, not too many breaks, and so on, all on the same day.
   (list
    ; TODO: Would it maybe be better to define step-handlers with normal arguments and pass them in?
    ; Rather than use (get 'n) inside? That way we can define more re-usable steps, while dealing with
    ; the nameing and `get`ting at the study level. E.g. make-step could do the mapping from 'required-tasks
    ; to the first argument or some such.
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
     ; TODO: Document how #:require-bindings and #:provide-bindings work
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
