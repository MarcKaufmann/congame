#lang racket

(require gregor
         gregor/time
         racket/match
         racket/serialize
         koyo/haml
         congame/components/formular
         congame/components/study)

(provide
 sleep-tracker)

; TODO: Provide overview of time per day slept.
; How to input sleeping time for days that are not the last night?
; TODO: Send daily email reminder to provide data
; TODO: Is it worth sending browser notifications to ask for data?
; TODO: How do we disambiguate data from different dates if I make it part of an ongoing study to track the information? How do we repeat the step every day, storing it in a new step? How can we trigger the next step by time if the person does not revisit the webpage? koyo/cron? Or is there another way?

; TODO: Can structs have optional arguments? I believe not.
; TODO: Do we need to do something special to store structs in the database across runs?
(serializable-struct sleep (from to awake duration)
  #:transparent)

(define (combine-date+time d t)
  (datetime (->year d)
            (->month d)
            (->day d)
            (->hours t)
            (->minutes t)
            (->seconds t)))

(define (sleep-question)
  (page
   (haml
    (.container
     (:h1 "Sleep Questionnaire")

     (formular
      (haml
       (:div
        (:div
         (#:fall-asleep-time (input-time "When did you fall asleep last night?"))
         (#:fall-asleep-date (input-date "")))
        (:div
         (#:wake-up-time (input-time "When did you wake up last night?"))
         (#:wake-up-date (input-date "")))
        (:div
         (#:awake-in-between (input-number "How long were you awake in between (in hours)?" #:min 0)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:fall-asleep-time fall-asleep-time
          #:fall-asleep-date fall-asleep-date
          #:wake-up-time wake-up-time
          #:wake-up-date wake-up-date
          #:awake-in-between awake-in-between)
        (displayln (list fall-asleep-time))
        (put 'fall-asleep-time fall-asleep-time)
        ;(displayln (list fall-asleep-date fall-asleep-time))
        ;(define fall-asleep
        ;  (combine-date+time
        ;   (parse-time fall-asleep-time "HH:mm")
        ;   ; FIXME: Once I know what the data looks like from input-date
        ;   (today)))
        ;(define wake-up
        ;  (combine-date+time
        ;   (parse-time wake-up-time "HH:mm")
        ;   ; FIXME: Once I know what the data looks like from input-date
        ;   (+days (today) 1)))
        ;(put 'sleep-records
        ;     ; FIXME: Once this runs
        ;     (cons (sleep fall-asleep wake-up 0 0)
        ;           (get 'sleep-records '())))
        ;(put 'fall-asleep-date fall-asleep-date)
        ))))))

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
