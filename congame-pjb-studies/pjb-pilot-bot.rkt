#lang racket/base

(require (submod congame/components/bot actions)
         congame/components/bot-maker
         racket/match
         "pjb-pilot.rkt")

(provide
 pjb-pilot-bot
 pjb-pilot-bot-model)

(define pjb-pilot-bot
  (study->bot pjb-pilot-study))

(define (pjb-pilot-bot-model id)
  (match id
    #;['(*root* elicit-WTW-and-work elicit-immediate-WTW)
       (willing-to-work? #t)]

    ['(*root* the-study tutorial-tasks task)
     #t]

    ['(*root* the-study required-tasks task)
     #t
     #;(begin0 (zero? c)
         (set! c (add1 c)))
     ]

    #;['(*root* price-list)
       1]

    ['(*root* the-study elicit-WTW-and-work extra-tasks task)
     #t]

    ['(*root* the-study elicit-WTW-and-work elicit-immediate-WTW)
     2]

    [_
     (values)]))

;; TODO: Give bots the ability to `get' (but maybe not put!) data.
(module+ main
  (run-bot
   #:study-url "http://127.0.0.1:5100/study/pilot1"
   #:username "bot@example.com"
   #:password "password"
   #:headless? #f
   #:delay 1
   (pjb-pilot-bot pjb-pilot-bot-model)))
