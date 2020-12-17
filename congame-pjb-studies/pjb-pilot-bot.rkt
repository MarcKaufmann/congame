#lang racket/base

(require congame/components/bot
         (submod congame/components/bot actions)
         marionette)

(define (task-bot/smart)
  (click 'correct-answer))

(define (task-bot/dumb)
  (click 'wrong-answer))

(define (make-required-tasks-bot task-bot)
  (make-bot
   (make-bot-stepper 'start-tasks continuer)
   (make-bot-stepper 'task task-bot)
   (make-bot-stepper 'success continuer)
   (make-bot-stepper 'failure continuer)))

(define (choose-price-lists)
  (define radios
    (page-query-selector-all! (current-page) "input[type=radio]"))
  (for ([elt (in-list radios)])
    (element-click! elt))
  (element-click! (page-wait-for! (current-page) "button[type=submit]")))

(define (make-price-lists-bot)
  (make-bot
   (make-bot-stepper 'info continuer)
   (make-bot-stepper 'price-list choose-price-lists)))

(define pjb-pilot-bot
  (make-bot
   (make-bot-stepper 'explain-study continuer)
   (make-bot-stepper 'tutorial continuer)
   (make-bot-stepper/study 'required-tasks (make-required-tasks-bot task-bot/smart))
   (make-bot-stepper 'get-rest continuer)
   (make-bot-stepper 'elicit-WTW continuer)
   (make-bot-stepper/study 'price-lists (make-price-lists-bot))
   (make-bot-stepper 'debrief-survey continuer)))

(module+ main
  (run-bot
   #:study-url "http://127.0.0.1:5100/study/pjb-pilot"
   #:username "bot@example.com"
   #:password "password"
   ;#:headless? #f
   pjb-pilot-bot))
