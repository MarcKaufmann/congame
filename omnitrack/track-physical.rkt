#lang racket

(require koyo/haml
         congame/components/study)

(provide
 sleep-tracker)

(define (sleep-question)
  (page
   (haml
    (.container
     (:h1 "Sleep Questionnaire")
     ; Qns to ask:
     ; 1. When fall asleep
     ; 2. When wake up
     ; 3. For how much awake in the middle
     (button void "Next")))))

(define (done-page)
  (page
   (haml
    (.container
     (:h1 "Done Page")
     (button void "Done")))))

(define sleep-tracker
  (make-study
   "sleep-tracker"
   #:requires '()
   #:provides '()
   (list
    (make-step 'sleep-question sleep-question)
    (make-step 'done-page done-page (λ () done)))))
