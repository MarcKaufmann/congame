#lang racket

(require koyo/haml
         congame/components/study
         congame/components/template
         (prefix-in bot: (submod congame/components/bot actions)))

(provide task-study)

(define (initialize-tasks)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "Start the tasks NOW!")
     (button
      (λ ()
        (put 'remaining-tasks (get 'n))
        (put 'correct-answers 0)
        (put 'wrong-answers 0))
      "Start Tasks")))))

; TODO: Split tasks out of this study into submodule of helper module
(define (task)
  ((page/xexpr)
   (haml
    (:div.container
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
      "I hAz no brAinZ...")))))

(define (task/bot correct?)
  (bot:click
   (if correct?
       'correct-answer
       'wrong-answer)))

(define (success)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "You GENIUS!")
     (button
      (λ () (put 'success? #t))
      "Continue")))))

(define (failure)
  ((page/xexpr)
   (haml
    (:div.container
     (:h1 "There, there...")
     (button
      (λ () (put 'success? #f))
      "The End")))))

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
