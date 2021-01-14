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

;; PjB Pilot has the following structure:
;;
;; 1. Sign-up
;; 2. Have study explained
;; 3. Tutorial
;; 4. Do [N] tasks
;; 5. Depending on treatment:
;;   - TIRED: Elicit WTW right away instead of extra rest, then rest
;;   - RESTED: rest, then elicit WTW right away instead of extra rest
;; 6. Debrief survey

(define (linear-handler title)
  (λ ()
    (haml
     (:div
      (:h1 title)
      (button void "Continue")))))

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
  void)

(define (send-completion-email)
  void)

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

(define task-completion
  (λ ()
    (cond [(zero? (get 'remaining-tasks)) 'success]
          [(> (get 'wrong-answers) 1) 'failure]
          [else 'task])))

(define task-study
  (make-study
   #:requires '(n)
   #:provides '(success? correct-answers wrong-answers)
   (list
    (make-step 'start-tasks initialize-tasks)
    (make-step 'task task #:for-bot task/bot task-completion)
    (make-step 'success success #:for-bot bot:continuer (λ () done))
    (make-step 'failure failure #:for-bot bot:continuer (λ () done)))))

(define pjb-pilot-study
  (make-study
   #:requires '()
   ; FIXME: How should we resolve the fact that for proper completion I expect WTW,
   ; but not when the person fails the study due to failing the tasks.
   ; Use a different state than `done`? Doesn't seem justified. Rather implement a way to set all required
   ; values to some kind of NA value, although maybe I should allow for error messages to be
   ; attached to data values -- i.e. reasons for failing?
   #:provides '(task-treatment rest-treatment)
   ;; TODO: Ensure work is done in appropriate time, i.e. all in one go, not too many breaks, and so on, all on the same day.
   (list
    (make-step
     'explain-study
     (linear-handler "Study Explanation"))
    (make-step 'tutorial tutorial)
    (make-step/study 'required-tasks
                     task-study
                     (lambda ()
                       (if (not (get 'success?))
                           done
                           (case (get 'rest-treatment)
                             [(get-rest-then-elicit) 'get-rest]
                             [(elicit-then-get-rest) 'elicit-WTW])))
                     #:require-bindings '([n task-treatment])
                     #:provide-bindings '([success? success?]))
    (make-step 'get-rest
               get-rest
               #:for-bot bot:continuer
               (lambda ()
                 (case (get 'rest-treatment)
                   [(get-rest-then-elicit) 'elicit-WTW]
                   [(elicit-then-get-rest) 'price-lists])))
    (make-step 'elicit-WTW
               elicit-WTW
               #:for-bot elicit-WTW/bot
               (lambda ()
                 (case (get 'rest-treatment)
                   [(get-rest-then-elicit) 'price-lists]
                   [(elicit-then-get-rest) 'get-rest])))
    (make-step/study 'price-lists pl-study)
    (make-step 'debrief-survey debrief-survey))))
