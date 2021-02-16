#lang racket/base

(require component
         (prefix-in forms: (only-in forms form))
         (except-in forms form)
         gregor
         marionette
         racket/contract
         racket/list
         racket/random
         koyo/haml
         koyo/job
         (prefix-in config: congame-web/config)
         congame/components/bot
         congame/components/study
         congame-pjb-studies/relax
         congame-price-lists/price-lists
         congame-web/components/sentry
         congame-web/components/mail
         congame/tools
         "tasks.rkt"
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 pjb-pilot-study)

(define (study-description required-tasks participation-fee)
  (haml
   (:div
    (:h2 "Main Study Description")
    (:p "If you decide to participate in the study, you will do the following:")
    (:ul
     (:li "complete " required-tasks " required tasks")
     (:li "choose whether to do additional tasks for bonus payments")
     (:li "do another 10-minute task involving sound/audio")
     (:li "fill in a brief survey"))
    (:p "If you complete the study, you will receive " (pp-money participation-fee) " as a participation fee, as well as any bonus payment from the additional tasks you choose."))))

(define (study-explanation)
  (define required-tasks (number->string (get 'required-tasks)))
  (define practice-tasks (number->string (get 'practice-tasks)))
  (define participation-fee (get 'participation-fee))
  (haml
   (:div.container.container
    (:h1 "Study Explanation")

    (:h2 "Tutorial")
    (:p "You are starting the tutorial for this study which consists of the following:")
    (:ul
     (:li "a study description (this page)")
     (:li "a page to check that your sound works")
     (:li "a description of the tasks in this study and doing " practice-tasks " practice tasks")
     (:li "a form asking whether you agree to participate in the study"))
    (:p "You will not receive any payment for completing the tutorial.")

    (study-description required-tasks participation-fee)
    (:p (:strong "Note:") " The study requires sound, so you need headphones or be able to listen to sound on your speakers." )

    (button void "Continue"))))

(define (consent)
  (define required-tasks (number->string (get 'required-tasks)))
  (define participation-fee (get 'participation-fee))
  (haml
   (:div.container
    (study-description required-tasks participation-fee)
    (render-consent-form))))

(define (test-comprehension)
  (haml
   (:div.container
    (:h1 "Comprehension Tests")
    (render-comprehension-form))))

(define (render-comprehension-form)
  (define the-form
    (form* ([understand? (ensure binding/text (required))])
           (list understand?)))
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
                   (button ((type "Submit") (class "button")) "Submit"))))))))

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
         ,(audio-container "test-audio.mp3" #:caption "What a song"))
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
  (element-click! (bot:find "#play"))
  (for ([checkbox (bot:find-all "input[type=checkbox]")])
    (displayln (format "checkbox is ~a" checkbox))
    (flush-output)
    (element-click! checkbox))
  ;; Use JS to submit the page faster; actually click the play button
  ;; and wait for the button to appear to simulate the real world.
  ;; FIXME: JS version to skip wait leads to checkbox clicking being done after the page is submitted,
  ;; leading to an error.
  #;(void
   (page-execute-async! (bot:current-page) "document.querySelector('form').submit()"))
  (element-click! (page-wait-for! (bot:current-page) "button[type=submit]")))

(define (test-study-requirements)
  (haml
   (:div.container
    (:h1 "Requirements for Study")
    (:p "Please check that you can play the test audio by hitting the play button, otherwise you cannot complete the study. Once the track has finished, a 'Continue' button will appear.")
    (render-requirements-form))))

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
  (haml
   (:div.container
    (:h1 "Payment Page")
    (:p "Within the next week, you will receive a total payment of " (get-total-payment) " for this study. The detailed breakdown is as follows:")
    (:ul
     ,@(for/list ([(name payment) (in-hash (get-all-payments))])
         (haml
          (:li (symbol->string name) ": " (pp-money payment)))))
    (:p "Shortly after finishing the study, you will receive an email from us. " (:a ((:href (string-append "mailto:" config:support-email))) "Email us") " if you have not received the payment by the end of next week." )
    (button
     (λ ()
       (send-completion-email (current-participant-id)))
     "Finish Study"))))

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
  (haml
   (form
    the-form
    (λ (survey-response)
      (put 'debrief-survey survey-response)
      (put-payment! 'participation-fee (get 'participation-fee)))
    (λ (rw)
      `(form ((action "")
              (method "POST"))
             (label
              "What is your gender?"
              ,(rw "gender" (widget-text)))
             ,@(rw "gender" (widget-errors))
             (button ((type "submit") (class "button next-button")) "Submit"))))))

(define (debrief-survey/bot)
  (define f (bot:find "form"))
  (for ([input (bot:element-find-all f "input")])
    (element-type! input "Bot, James Bot"))
  (element-click! (bot:find "button[type=submit]")))

(define (debrief-survey)
  (haml
   (:div.container
    (:h1 "Debrief Survey")
    (render-debrief-form))))

(require (for-syntax racket/base) ; Needed to use strings in define-static-resource. Why? Just Cause.
         congame/components/resource)

;; File resources:
(define-static-resource song1 (build-path "songs" "song1.mp3"))

(define stepsize 0.2)
(define money-levels
  (build-list 10 (λ (i) (* i stepsize))))
(define (pl-extra-tasks t name)
  (make-pl #:name name
           #:fixed-work 0
           #:fixed-money 0
           #:adjustable-work t
           #:levels-of-money money-levels))

(define PRICE-LISTS
  (hash 'pl10 (pl-extra-tasks 10 'pl10)
        'pl7  (pl-extra-tasks 7 'pl7)
        'pl5  (pl-extra-tasks 5 'pl5)))

(define (determine-extra-tasks)
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
       (displayln (format "wtws: ~a" wtws))
       (flush-output)
       (put 'WTWs wtws)
       (define pl-that-counts (random-ref pls))
       (put 'price-list-that-counts pl-that-counts)
       (define pl/answers (get pl-that-counts))
       (define pl/answers+choice (pl-random-choice pl/answers))
       ; Store price list with the choice that counts. TODO: Dangerous to overwrite original, no?
       (put 'choice-that-counts pl/answers+choice)
     "See extra tasks")))))

(define (see-extra-tasks)
  (define pl/answers+choice (get 'choice-that-counts))
  (define chosen (price-list-chosen pl/answers+choice))
  (define alternative (price-list-alternative pl/answers+choice))
  (define extra-tasks (option-work chosen))
  (define extra-bonus (pp-money (option-money chosen)))
  (define continue-text
    (if (> extra-tasks 0)
        (format "Continue to ~a extra tasks" extra-tasks " for a " extra-bonus " extra bonus.")
        "Continue with no extra tasks or extra bonus."))
  (haml
   (:div.container
    (:h1 "Extra Tasks")
    (:p "The choice that was randomly selected was between the following two options, from which you chose the one listed first:")
    (:ol
     (:li (describe chosen))
     (:li (describe alternative)))
    (:p continue-text)
    (button void continue-text))))

(define (introduce-WTW)
  (define pls (get 'price-lists))
  (define n (length pls))
  (haml
   (.container
    (:h1 "Extra Effort Choices")
    (:p "On the next " (number->string n) " pages, you will make choices about doing extra tasks for bonus payments. After you have made your choices, the computer randomly picks one of the pages, and one of the choices on that page as the choice that counts. If for that choice you picked the extra tasks over no extra tasks, then you have to do the extra tasks and will receive the extra payment. If you do not, you fail the study and will not receive the bonus nor the completion fee.")
    (button
     (λ ()
       (put 'remaining-price-lists (shuffle pls))
       (put 'answered-price-lists '()))
     "Continue"))))

(define (task-failure)
  (haml
   (:div.container
    (:h1 "You failed the tasks")
    (:p "You failed the tasks and cannot continue the study.")
    ; TODO: Improve how to deal with failures
    #;(button void "The end")
    )))

(define elicit-WTW-and-work
  (make-study
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
                          (put-payment! 'extra-tasks-bonus (get 'extra-money))
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
  (haml
   (:div.container
    (:h1 "You do not satisfy the requirements")
    (:p "You fail some of the requirements for the study, therefore you cannot complete the study.")
    (button void "The End"))))

(define (consent-failure)
  (haml
   (:div.container
    (:h1 "You did not consent")
    (:p "You did not consent to the study, therefore you cannot complete the study.")
    (button void "The End"))))

(define pjb-pilot-study-no-config
  (make-study
   #:requires '(participation-fee practice-tasks required-tasks price-lists)
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
     #:require-bindings '([n practice-tasks]
                          [max-wrong-tasks practice-tasks]
                          [title (const "Practice Tasks")]
                          [hide-description? (const #f)])
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
    (make-step 'debrief-survey debrief-survey #:for-bot debrief-survey/bot)
    (make-step 'show-payments show-payments (λ () done) #:for-bot bot:continuer)
    (make-step 'task-failure task-failure (λ () done) #:for-bot bot:continuer)
    (make-step 'requirements-failure requirements-failure (λ () done) #:for-bot bot:continuer)
    (make-step 'consent-failure consent-failure (λ () done) #:for-bot bot:continuer)
    )))

(define pjb-pilot-study
  (make-study
   #:requires '()
   #:provides '(task-treatment rest-treatment)
   (list
    (make-step/study 'the-study
                     pjb-pilot-study-no-config
                     #:provide-bindings '([task-treatment task-treatment]
                                          [rest-treatment rest-treatment])
                     #:require-bindings `([practice-tasks (const 2)]
                                          [participation-fee (const 2.00)]
                                          [required-tasks (const 3)]
                                          [price-lists (const ,(hash-keys PRICE-LISTS))])))))
