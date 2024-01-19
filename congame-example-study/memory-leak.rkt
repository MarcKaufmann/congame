#lang racket/base

(require koyo/haml
         congame/components/study
         congame/components/transition-graph
         congame/components/formular
         (submod congame/components/formular tools))

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

(define (quiz)
  (page
   (haml
    (.container
     (:h1 "Quiz")
     (formular
      (#:n (input-number "Pick a random number between 0 and 100."))
      submit-button)))))

(define memory-leak
  (make-study
   "memory-leak"
   (append
    (list
     (make-step 'start start))
    (build-list
     (lambda (i)
       (make-step
        (string->symbol
         (format "quiz~a" i))
        quiz)))
    (list
     (make-step 'end end)))))
