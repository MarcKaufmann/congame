#lang racket/base

(require (submod congame/components/bot actions)
         congame/components/bot-maker
         racket/match
         "pjb-pilot.rkt")

(provide
 make-pjb-pilot-bot
 pjb-pilot-bot-model/full
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
    ['(*root* tutorial-tasks task)
     ;; NOTE: We don't _have to_ call bot here and instead could take
     ;; our own bot actions (from the congame/components/bot actions
     ;; submodule).
     (bot #t)]

    ['(*root* required-tasks task)
     (bot #t)]

    ['(*root* elicit-WTW-and-work extra-tasks task)
     (bot #t)]

    ['(*root* elicit-WTW-and-work elicit-immediate-WTW)
     (bot 2)]

    [_
     (bot)]))

(define (pjb-pilot-bot-model/full id bot)
  (match id
    [`(*root* the-study . ,path)
     (pjb-pilot-bot-model (cons '*root* path) bot)]

    [_
     (bot)]))

;; While bots can't access instance data, they still have the ability
;; to introspect the same things a normal human could.  So, in the
;; multi-review example, while bots can't know ahead of time who they
;; will be matched with, they can inspect the design files and make
;; decisions based on that.
(define current-multi-bot-id (make-parameter #f))
(define (make-multi-bot)
  (define sema (make-semaphore 1))
  (define bots (make-hash))
  (define seq 0)
  (define (next-bot-id)
    (begin0 seq
      (set! seq (add1 seq))))
  (lambda (id bot)
    (call-with-semaphore sema
      (lambda ()
        (match id
          ['(*root* the-study arrive)
           (current-multi-bot-id (next-bot-id))
           (hash-set! bots (next-bot-id) 'initial-state)
           (bot)]
          ['(*root* the-study step-2)
           (current-multi-bot-id)
           (bot)]
          [_
           (bot)])))))

(module+ main
  #;
  (let ()
    (define thds
      (for/list ([_ (in-range 2)])
        (run-bot
         (make-multi-bot))))
    (for-each thread-wait thds))

  (time
   (run-bot
    #:study-url "http://127.0.0.1:5100/study/pilot1"
    #:username "bot@example.com"
    #:password "password"
    #:headless? #f
    (make-pjb-pilot-bot pjb-pilot-bot-model/full))))
