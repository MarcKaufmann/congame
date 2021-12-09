#lang racket/base

(require (submod congame/components/bot actions)
         congame/components/bot-maker
         racket/match
         "pjb-pilot.rkt")

(provide
 make-pjb-pilot-bot
 pjb-pilot-bot-model
 make-relax-test-bot
 relax-test-bot-model)

(define make-pjb-pilot-bot
  (study->bot pjb-pilot-study))

(define make-relax-test-bot
  (study->bot relax-test-study))

(define (relax-test-bot-model id bot)
  (match id
    [_ (bot)]))

(define (pjb-pilot-bot-model id bot)
  (match id
    ['(*root* the-study tutorial-tasks task)
     (bot #t)]

    ['(*root* the-study required-tasks task)
     (bot #t)]

    ['(*root* the-study elicit-WTW-and-work extra-tasks task)
     (bot #t)]

    ['(*root* the-study elicit-WTW-and-work elicit-immediate-WTW)
     (bot 2)]

    [_
     (bot)]))

(module+ main
  (time
   (run-bot
    #:study-url "http://127.0.0.1:5100/study/pilot1"
    #:username "bot@example.com"
    #:password "password"
    #:headless? #t
    (make-pjb-pilot-bot pjb-pilot-bot-model))))
