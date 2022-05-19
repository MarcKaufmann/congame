#lang racket/base

(require congame/components/bot-maker
         congame/components/study
         koyo/haml)

(provide
 test-substudy-failing-failed-step
 test-substudy-failing
 make-test-substudy-failing-bot)

(define test-substudy-failing-failed-step
  (box #f))

(define (info)
  (page (button void "Continue")))

(define (done)
  (page (haml (:p "Done."))))

(define (do-fail)
  (page (button
         (λ ()
           (fail 'expected-failure))
         "Fail")))

(define test-substudy-failing/child1
  (make-study
   "test-substudy-failing/child1"
   (list
    (make-step 'fail1 do-fail))))

(define test-substudy-failing/child2
  (make-study
   "test-substudy-failing/child2"
   (list
    (make-step 'child2-info info))))

(define test-substudy-failing
  (make-study
   "test-substudy-failing/parent"
   #:failure-handler (λ (the-step err)
                       (set-box! test-substudy-failing-failed-step (cons the-step err))
                       'child2)
   (list
    (make-step 'info info)
    (make-step/study 'child1 test-substudy-failing/child1)
    (make-step/study 'child2 test-substudy-failing/child2)
    (make-step 'done done))))

(define make-test-substudy-failing-bot
  (study->bot test-substudy-failing))
