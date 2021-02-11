#lang racket/base

(require (for-syntax racket/base)
         (except-in forms form)
         racket/match
         racket/random
         racket/runtime-path
         racket/serialize
         racket/string
         koyo/haml
         marionette
         congame/components/study
         congame/components/resource
         "generate-matrices.rkt"
         (prefix-in config: congame-web/config)
         congame/components/bot
         (prefix-in bot: (submod congame/components/bot actions)))

(provide task-study)

;; Generate the matrices if there is no csv mapping to them
(define-static-resource matrix-dir "matrices")
(define-runtime-path matrix-csv "matrix.csv")

(define n-matrices 5)

; TODO: Why does `(current-directory)` yield congame instead of congame/congame-pjb-studies?
; And why do relative paths (such as "matrix.csv") not work with `file-exists?` at run-time?
(unless (file-exists? matrix-csv)
  (list->file
   (list-of-large-matrices
    n-matrices
    (resource-path matrix-dir))
   matrix-csv))

; TODO: Is this toggleable or only shows?
(define (toggleable-xexpr message xexpr #:hidden? [hidden? #t])
  (haml
   (:div
    ([:class (if hidden?
                 "toggleable toggleable--hidden"
                 "toggleable")])
    (:button.toggleable__toggle message)
    (.toggleable__content xexpr))))

(define (task-description)
  (haml
   (.container.info
    (:h3 "Matrix Task Description")
    (:p "Each page will look like the screenshot below.")
    (:ul
     (:li "You have to count the number of cells that contain exactly the number 1")
     (:li "Cells containing the number 11 or 10 do not count")
     (:li "If you get more tasks wrong than you have to get right, you fail the tasks and thus the tutorial")
     (:li "If you get a matrix wrong, you will be given a new one."))
    (.container.screenshot
     (:h2 "Screenshot of Toy Matrix")
     (:h3 "Count only cells with exactly \"1\" in it")
     (:p "In this toy 2 by 2 matrix, only 1 cell contains exactly the number 1. The cells containing 10 and 11 do not count:")
     (:img ([:src (resource-uri matrix-dir "matrix-screenshot.png")]))))))

; TODO: Write down that resource-uri will lead to stub if using a constant computed at compile time...
;; Load the matrix data

(serializable-struct matrix (id answer file) #:transparent)

(define MATRICES
  (call-with-input-file matrix-csv
    (λ (in)
      (for/list ([line (in-lines in)])
        (match-define (list id answer file)
          (string-split line ","))
        (matrix (string->number id)
                (string->number answer)
                file)))))

(define (random-matrix)
  (random-ref MATRICES))

(define (initialize-tasks)
  (define n (get 'n))
  (define title (get 'title))
  (define n-string (number->string n))
  (define hide-description? (get 'hide-description?))
  (haml
   (.container
    (:h1 title)
    (:p "You now have to do " n-string " tasks successfully, and you can get at most " n-string " wrong. If you get more wrong, you automatically fail and drop out of the study.")
    (toggleable-xexpr "Show Task Description" (task-description) #:hidden? hide-description?)
    (button
     (λ ()
       (put 'remaining-tasks (get 'n))
       (put 'correct-answers 0)
       (put 'wrong-answers 0)
       (put 'current-matrix (random-matrix)))
     "Start Tasks"))))

(define (task)
  (define m (get 'current-matrix))
  (displayln (list "matrix in task: " m))
  (flush-output)
  (define (correct-answer? m n)
    (displayln (list "matrix in correct-answer?" m))
    (flush-output)
    (= (matrix-answer m) n))
  (define task-form
    (form* ([number-of-ones (ensure binding/number (required))])
           number-of-ones))
  (define submit-action
    (λ (number-of-ones)
      (define m (get 'current-matrix))
      (displayln (list "answer provided:" number-of-ones))
      (flush-output)
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
  ;; BUG: form doesn't run the initial submit-action, but the one that gets created
  ;; on the next call to the whole handler or step!! That seems pretty wrong.
  ;; While one can work around it, it will inevitably lead to hard-to-find bugs, and
  ;; is conceptually wrong.
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
        (:label
         "How many cells with the number 1 are in the matrix? (Note: cells with 01, 10, or 11 do not count.)"
         (rw "number-of-ones" (widget-number)))
        ,@(rw "number-of-ones" (widget-errors))
        (:button.button ([:type "submit"]) "Submit"))
       (when config:debug
         (haml
          (.container.debug
           (:p "Answer: " (number->string (matrix-answer m))))))
       (when (current-user-bot?)
         (haml
          (.container
           (:p ([:data-answer (number->string (matrix-answer m))]) "")))))))))

(define (task/bot correct?)
  (define answer (bot:find-attribute "data-answer"))
  (define input
    (bot:element-find
     (bot:find "form")
     "input"))
  (element-type! input answer)
  (element-click! (bot:find "button[type=submit]")))

(define (task-completion)
  (cond [(<= (get 'remaining-tasks) 0)
         (put 'success? #t)
         done]
        [(> (get 'wrong-answers) (get 'max-wrong-tasks))
         (put 'success? #f)
         done]
        [else
         (put 'current-matrix (random-matrix))
         'task]))

(define task-study
  (make-study
   #:requires '(n title max-wrong-tasks hide-description?)
   #:provides '(success? correct-answers wrong-answers)
   (list
    (make-step 'start-tasks
               initialize-tasks
               task-completion
               #:for-bot bot:continuer)
    (make-step 'task task #:for-bot task/bot task-completion))))
