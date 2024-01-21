#lang racket/base

(require racket/list
         koyo/haml
         congame/components/bot-maker
         congame/components/study
         congame/components/transition-graph
         congame/components/formular
         (submod congame/components/formular tools)
         (prefix-in bot: (submod congame/components/bot actions)))

(provide
 memory-leak)

(define (start)
  (page
   (haml
    (.container
     (:h1 "Start")
     (button void "Continue")))))

(define (end)
  (page
   (haml
    (.container
     (:h1 "End")))))

(define ((quiz i))
  (page
   (haml
    (.container
     (:h1 (format "Quiz ~a" i))
     (formular
      #:bot ([random (#:n (random 0 100))])
      (haml
       (:div
        (:div
         (#:n (input-number #:min 0 #:max 100 "Pick a random number between 0 and 100.")))
        submit-button))
      (make-put-form/cons))))))

(define (quiz/bot)
  (formular-autofill 'random))

(define memory-leak
  (make-study
   "memory-leak"
   (append
    (list
     (make-step 'start start))
    (build-list
     20
     (lambda (i)
       (make-step
        (string->symbol
         (format "quiz~a" i))
        (quiz i))))
    (list
     (make-step 'end end #:for-bot bot:completer)))))

; Bots

(provide
 make-ml-bot
 ml-bot-model)

(define make-ml-bot
  (study->bot memory-leak))

(define ml-bot-model
  (lambda (_id bot)
    (bot)))

(module+ main
  (time
   (bot:run-bot
    #:study-url "http://127.0.0.1:5100/study/memory-leak"
    #:username "bot@example.com"
    #:password "password"
    #:headless? #f
    (make-ml-bot ml-bot-model))))
