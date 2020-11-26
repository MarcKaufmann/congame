#lang racket/base

(require racket/contract
         racket/list
         racket/match
         (except-in forms form)
         koyo/haml
         "../components/registry.rkt"
         "../components/study.rkt")

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
     (λ () (put 'WTW 5))
     "Continue"))))

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

(define/contract (match/treatment treatment hot)
  (-> symbol? (hash/c symbol? symbol?) (-> symbol?))
  (λ ()
    (hash-ref hot (get treatment))))

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
     (λ ()
       (put 'remaining-tasks (sub1 (get 'remaining-tasks)))
       (put 'correct-answers (add1 (get 'correct-answers))))
     "Well done")
    (:br)
    (button
     (λ ()
       (put 'wrong-answers (add1 (get 'wrong-answers))))
     "I hAz no brAinZ..."))))

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
    (make-step 'task task task-completion)
    (make-step 'success success (λ () done))
    (make-step 'failure failure (λ () done)))))

(define pjb-pilot-study
  (make-study
   #:requires '()
   #:provides '(WTW task-treatment rest-treatment)
   ;; TODO: Ensure work is done in appropriate time, i.e. all in one go, not too many breaks, and so on, all on the same day.
   (list
    (make-step 'explain-study (linear-handler "Study Explanation"))
    (make-step 'tutorial tutorial)
    (make-step/study 'required-tasks
                     task-study
                     ; FIXME: Should fork depending on 'success? being #t or #f, no idea how to access right now
                     (match/treatment
                      'rest-treatment
                      (hash 'get-rest-then-elicit 'get-rest
                            'elicit-then-get-rest 'elicit-WTW))
                     #:require-bindings '([n task-treatment])
                     #:provide-bindings '())
    (make-step 'get-rest
               get-rest
               (match/treatment
                'rest-treatment
                (hash 'get-rest-then-elicit 'elicit-WTW
                      'elicit-then-get-rest 'debrief-survey)))
    (make-step 'elicit-WTW
               elicit-WTW
               (match/treatment
                'rest-treatment
                (hash 'get-rest-then-elicit 'debrief-survey
                      'elicit-then-get-rest 'get-rest)))
    (make-step 'debrief-survey debrief-survey))))

(register-study! 'pjb-pilot pjb-pilot-study)
