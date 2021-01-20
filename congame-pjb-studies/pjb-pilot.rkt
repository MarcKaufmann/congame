#lang racket/base

(require racket/contract
         racket/list
         (except-in forms form)
         koyo/haml
         congame/components/study
         congame-price-lists/price-lists
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 pjb-pilot-study)

(define (study-explanation)
  (haml
   (:div
    (:h1 "Study Explanation")
    (button void "Continue"))))

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

(define (tutorial)
  (haml
   (:div
    (:h1 "Tutorial")
    (button
     (λ ()
       (put 'rest-treatment (next-balanced-rest-treatment))
       (put 'task-treatment (next-balanced-task-treatment)))
     "Finish Tutorial"))))

(define (compute-payments)
  ; TODO: implement
  (haml
   (:div
    (:h1 "Your Payment")
    (:p "... (TBC)")
    (button void "Continue"))))

(define (send-completion-email)
  ; TODO: implement
  (haml
   (:div
    (:h1 "Sending completion email")
    (button void "Continue"))))

(define (debrief-survey)
  (haml
   (:div
    (:h1 "Debrief Survey")
    (button
     (λ ()
       (compute-payments)
       (send-completion-email))
     "The End"))))

(define (required-tasks)
  (haml
   (:div
    (:h1 "Required Tasks")
    (button void "Continue"))))

(define (get-rest)
  (haml
   (:div
    (:h1 "Take a break")
    (button void "Continue"))))

(define (initialize-tasks)
  (haml
   (:div
    (:h1 "Start the tasks NOW!")
    (button
     (λ ()
       (put 'remaining-tasks (get 'n))
       (put 'correct-answers 0)
       (put 'wrong-answers 0))
     "Start Tasks"))))

; TODO: Split tasks out of this study into submodule of helper module
(define (task)
  (haml
   (:div
    (:h1 "Click button that says 'Well done' for task completion!")
    (button
     #:id 'correct-answer
     (λ ()
       (put 'remaining-tasks (sub1 (get 'remaining-tasks)))
       (put 'correct-answers (add1 (get 'correct-answers))))
     "Well done")
    (:br)
    (button
     #:id 'wrong-answer
     (λ ()
       (put 'wrong-answers (add1 (get 'wrong-answers))))
     "I hAz no brAinZ..."))))

(define (task/bot correct?)
  (bot:click
   (if correct?
       'correct-answer
       'wrong-answer)))

(define (success)
  (haml
   (:div
    (:h1 "You GENIUS!")
    (button
     (λ () (put 'success? #t))
     "Continue"))))

(define (failure)
  (haml
   (:div
    (:h1 "There, there...")
    (button
     (λ () (put 'success? #f))
     "The End"))))

(define (task-completion)
  (cond [(<= (get 'remaining-tasks) 0) 'success]
        [(> (get 'wrong-answers) 1) 'failure]
        [else 'task]))

(define task-study
  (make-study
   #:requires '(n)
   #:provides '(success? correct-answers wrong-answers)
   (list
    (make-step 'start-tasks
               initialize-tasks
               task-completion
               #:for-bot bot:continuer)
    (make-step 'task task #:for-bot task/bot task-completion)
    (make-step 'success success #:for-bot bot:continuer (λ () done))
    (make-step 'failure failure #:for-bot bot:continuer (λ () done)))))

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

(define pjb-pilot-study
  (make-study
   #:requires '()
   ; FIXME: #:provides should include WTW, but we won't get that if the person fails the tasks.
   ; Upon failing a study, call/set some default values for provide, error codes attached to data or NA.
   #:provides '(task-treatment rest-treatment)
   ;; TODO: Ensure work is done in appropriate time, i.e. all in one go, not too many breaks, and so on, all on the same day.
   (list
    (make-step 'explain-study study-explanation)
    (make-step 'tutorial tutorial)
    (make-step/study 'required-tasks
                     task-study
                     (λ ()
                       (if (not (get 'success?))
                           done
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
    (make-step 'debrief-survey debrief-survey)
    (make-step 'compute-payments compute-payments)
    (make-step 'send-completion-email send-completion-email)
    )))
