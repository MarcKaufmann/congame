#lang racket/base

(require congame/components/bot-maker
         congame/components/study
         koyo/haml)

;; The point of this test is to check that we are able to catch a
;; failure then run the substudy again, then catch the failure again,
;; repeat, and eventually succeed.  The idea is to ensure exception
;; handlers don't "disappear" after use.

(provide
 test-looping-failures
 make-test-looping-failures-bot)

(define (cont)
  (button void "Continue"))

(define (done)
  (haml (:p "Done.")))

(define fail-count 0)
(define (maybe-fail)
  (when (< fail-count 2)
    (set! fail-count (add1 fail-count))
    (fail 'expected-failure))
  (button void "Continue"))

(define test-looping-failures-child
  (make-study
   "test-looping-failures-child"
   (list
    (make-step 'fail maybe-fail))))

(define test-looping-failures
  (make-study
   "test-looping-failures"
   #:failure-handler (λ (_the-step _err)
                       'child)
   (list
    (make-step 'cont cont)
    (make-step/study 'child test-looping-failures-child)
    (make-step 'done done))))

(define make-test-looping-failures-bot
  (study->bot test-looping-failures))
