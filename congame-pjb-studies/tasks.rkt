#lang racket

(require (except-in forms form)
         racket/random
         koyo/haml
         congame/components/study
         congame/components/resource
         (prefix-in bot: (submod congame/components/bot actions)))

(provide task-study)

(define task-description
  (haml (:p "Description")))

(define (initialize-tasks)
  (define n (get 'n))
  (define n-string (number->string n))
  (haml
   (.container
    (:h1 "Doing Tasks")
    (:p "You now have to do " n-string " tasks successfully, and you can get at most " n-string " wrong. If you get more wrong, you automatically fail and drop out of the study.")
    task-description
    (button
     (λ ()
       (put 'remaining-tasks (get 'n))
       (put 'correct-answers 0)
       (put 'wrong-answers 0))
     "Start Tasks"))))

(define-static-resource matrix-dir "matrices")

(define (task m)
  (define (correct-answer? m n)
    (= (matrix-answer m) n))
  (define task-form
    (form* ([number-of-ones (ensure binding/number (required))])
           number-of-ones))
  (define submit-action
    (λ (number-of-ones)
      ; TODO: Should I store all answers participants give in a DB table for the task study?
      ; We should develop a system how substudies can create required tables if needed
      ; Why needed? Because I want to have a table that has current-state, matrix-id, and answer given,
      ; which cannot be done with a simple put, which takes only a single symbolic key that needs to
      ; be unique.
      (cond [(correct-answer? m number-of-ones)
             (put 'remaining-tasks (sub1 (get 'remaining-tasks)))
             (put 'correct-answers (add1 (get 'correct-answers)))]
            [else
             (put 'wrong-answers (add1 (get 'wrong-answers)))])))
  (form
   task-form
   submit-action
   (λ (rw)
     (define tasks-remaining (get 'remaining-tasks))
     (define tasks-correct (get 'correct-answers))
     (define tasks-wrong (get 'wrong-answers))
     (define max-tasks-wrong (get 'max-wrong-tasks))
     (define tasks-total
       (+ tasks-remaining tasks-correct))
     (haml
      (.container
       (:h1 "Count the cells with 1's in them")
       (:p (format "You completed ~a out of ~a tasks (~a wrong guesses out of at most ~a)"
                   tasks-correct
                   tasks-total
                   tasks-wrong
                   max-tasks-wrong))
       (:p (format "If you get more than ~a wrong guesses, you drop out of the study." max-tasks-wrong))
       (.matrix
        (:img.matrix ([:src (resource-uri matrix-dir (matrix-file m))])))
       (:form
        ([:action ""]
         [:method "POST"])
        (:label
         "How many cells with the number 1 are in the matrix? (Note: cells with 01, 10, or 11 do not count.)"
         (rw "number-of-ones" (widget-number)))
        ,@(rw "number-of-ones" (widget-errors))
        (:button ([:type "submit"]) "Submit")))))))

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
        [(> (get 'wrong-answers) (get 'max-wrong-tasks)) 'failure]
        [else 'task]))

(struct matrix (id answer file) #:transparent)

(define matrices
  (list (matrix 1 1 "matrix1.png")))

(define (random-matrix)
  (random-ref matrices))

(define task-study
  (make-study
   #:requires '(n)
   #:provides '(success? correct-answers wrong-answers)
   (list
    (make-step 'start-tasks
               initialize-tasks
               task-completion
               #:for-bot bot:continuer)
    (make-step 'task (λ ()
                       (task (random-matrix)))
               #:for-bot task/bot
               task-completion)
    (make-step 'success success #:for-bot bot:continuer (λ () done))
    (make-step 'failure failure #:for-bot bot:continuer (λ () done)))))
