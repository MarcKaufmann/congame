#lang racket/base

(require (submod congame/components/bot actions)
         congame/components/bot-maker
         racket/match
         "pjb-pilot.rkt")

(define pjb-pilot-bot
  (study->bot pjb-pilot-study))

;; TODO: Give bots the ability to `get' (but maybe not put!) data.
(module+ main
  (define (model id)
    (match id
      ['(*root* elicit-WTW)
       (willing-to-work? #t)]

      ['(*root* required-tasks task)
       #t
       #;
       (begin0 (zero? c)
         (set! c (add1 c)))]

      ['(*root* price-list)
       1]

      [_
       (values)]))

  (run-bot
   #:study-url "http://127.0.0.1:5100/study/pilot1"
   #:username "bot@example.com"
   #:password "password"
   ;; #:headless? #f
   ;; #:delay 1
   (pjb-pilot-bot model)))
