#lang racket

(require koyo/haml
         congame/components/formular
         congame/components/study)

(provide
 sleep-tracker)

; TODO: Provide overview of time per day slept.
; How to input sleeping time for days that are not the last night?
; TODO: Send daily email reminder to provide data
; TODO: Is it worth sending browser notifications to ask for data?
; TODO: How do we disambiguate data from different dates if I make it part of an ongoing study to track the information? How do we repeat the step every day, storing it in a new step? How can we trigger the next step by time if the person does not revisit the webpage? koyo/cron? Or is there another way?

(define (sleep-question)
  (page
   (haml
    (.container
     (:h1 "Sleep Questionnaire")

     (formular
      (haml
       (:div
        (:div
         (#:fall-asleep (input-time "When did you fall asleep last night?")))
        (:div
         (#:wake-up (input-time "When did you wake up last night?")))
        (:div
         (#:awake-in-between (input-number "How long were you awake in between (in hours)?" #:min 0)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:fall-asleep fall-asleep
          #:wake-up wake-up
          #:awake-in-between awake-in-between)
        (put 'fall-asleep fall-asleep)
        (put 'wake-up wake-up)
        (put 'awake-in-between awake-in-between)))))))

(define (overview-page)
  (page
   (haml
    (.container
     (:h1 "Overview Page")
     (button void "Enter Sleep Data" #:to-step-id 'sleep-question)))))

(define sleep-tracker
  (make-study
   "sleep-tracker"
   #:requires '()
   #:provides '()
   (list
    (make-step 'overview-page overview-page)
    (make-step 'sleep-question sleep-question (λ () 'overview-page)))))
