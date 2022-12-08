#lang racket/base

(require (except-in congame/components/study fail)
         congame/components/transition-graph
         rackunit)

(provide
 study-tests)

(define study-tests
  (test-suite
   "study"

   (test-case "step with valid transition targets"
     (check-not-false
      (make-study
       "valid"
       #:transitions
       (transition-graph
        [a --> b --> ,done])
       (list
        (make-step 'a void)
        (make-step 'b void)))))

   (test-case "step with invalid transition target"
     (check-exn
      #rx"expected a known step\n  unknown step: c\n  known steps: a, b"
      (lambda ()
        (make-study
         "invalid"
         #:transitions
         (transition-graph
          [a --> c]
          [b --> a]
          [c --> b])
         (list
          (make-step 'a void)
          (make-step 'b void))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests study-tests))
