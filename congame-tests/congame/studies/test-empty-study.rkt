#lang racket/base

(require congame/components/study
         koyo/haml)

(provide
 test-empty-study)

(define (done)
  (haml (:p#study-done "Done.")))

(define test-empty-study
  (make-study
   "test-empty-study"
   (list
    (make-step 'done done))))
