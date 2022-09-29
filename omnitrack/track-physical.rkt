#lang racket

(require component
         gregor
         gregor/time
         racket/match
         racket/serialize
         koyo/haml
         koyo/job
         koyo/random
         congame/components/export
         congame/components/formular
         congame/components/study)

(provide
 sleep-tracker)

; TODO: Provide overview of time per day slept.
; How to input sleeping time for days that are not the last night?
; TODO: Send daily email reminder to provide data
; TODO: Is it worth sending browser notifications to ask for data?

; TODO: Document that data structs have to be serializable to be
; stored in the db.
(serializable-struct sleep (from to awake duration)
  #:transparent
  #:methods gen:jsexprable
  [(define (->jsexpr s)
     (match-define (sleep from to awake duration) s)
     (hasheq 'from (datetime->iso8601 from)
             'to (datetime->iso8601 to)
             'awake awake
             'duration duration))])

(define (combine-date+time d t)
  (datetime (->year d)
            (->month d)
            (->day d)
            (->hours t)
            (->minutes t)
            (->seconds t)))

(define (sleep-question)
  (define nonce (generate-random-string))
  (with-study-transaction
    (parameterize ([current-study-stack null])
      (define nonces (get/instance 'nonces hasheqv))
      (put/instance 'nonces (hash-set nonces (current-participant-id) nonce))))

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
        (displayln (list fall-asleep-date fall-asleep-time))
        (define fall-asleep
          (combine-date+time
           (today)
           (parse-time fall-asleep-time "HH:mm")
           ; FIXME: Once I know what the data looks like from input-date
           ))
        (define wake-up
          (combine-date+time
           (+days (today) 1)
           (parse-time wake-up-time "HH:mm")
           ; FIXME: Once I know what the data looks like from input-date
           ))
        (put 'sleep-records
             ; FIXME: Once this runs
             (cons (sleep fall-asleep wake-up 0 0)
                   (get 'sleep-records '())))
        (put 'fall-asleep-date fall-asleep-date)
        (schedule-at
         (+minutes (now/moment) 1)
         (request-update
          (current-participant-id)
          nonce))))))))

(define-job (request-update pid nonce)
  (call-with-study-manager
   (let ([db (system-ref 'db)])
     (make-study-manager
      #:database db
      #:participant (lookup-study-participant/by-id db pid)))
   (lambda ()
     (define nonces
       (parameterize ([current-study-stack null])
         (get/instance 'nonces hasheqv)))
     (when (equal? (hash-ref nonces pid) nonce)
       (println `(emailing ,pid ,nonce))
       (schedule-at
        (+minutes (now/moment) 1)
        (request-update pid nonce))))))

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
