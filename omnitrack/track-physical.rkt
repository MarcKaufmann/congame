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
    (parameterize ([current-study-stack '(*root*)])
      (define nonces (get/instance 'nonces hasheqv))
      (put/instance 'nonces (hash-set nonces (current-participant-id) nonce))
      (schedule-at
       (+minutes (now/moment) 1)
       (request-update
        (current-participant-id)
        nonce))))

  (page
   (haml
    (.container
     (:h1 "Sleep Times Last Night")

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
        (define fall-asleep
          (combine-date+time
           (parse-date fall-asleep-date "yyyy-MM-dd")
           (parse-time fall-asleep-time "HH:mm")))
        (define wake-up
          (combine-date+time
           (parse-date wake-up-date "yyyy-MM-dd")
           (parse-time wake-up-time "HH:mm")))
        (put 'sleep-records
             (cons (sleep fall-asleep wake-up awake-in-between (/ (seconds-between fall-asleep wake-up) 3600.0))
                   (get 'sleep-records '())))
        (put 'fall-asleep-date fall-asleep-date)))))))

(define (send-email-reminder pid nonce)
  ; FIXME
  (println `(emailing ,pid ,nonce)))

(define-job (request-update pid nonce)
  (call-with-study-manager
   (let ([db (system-ref 'db)])
     (make-study-manager
      #:database db
      #:participant (lookup-study-participant/by-id db pid)))
   (lambda ()
     (parameterize ([current-study-stack '(*root*)])
       (define nonces
         (get/instance 'nonces hasheqv))
       (when (equal? (hash-ref nonces pid) nonce)
         (send-email-reminder pid nonce)
         (define new-nonce (generate-random-string))
         (put/instance 'nonces (hash-set nonces pid new-nonce))
         (schedule-at
          (+minutes (now/moment) 1)
          (request-update pid new-nonce)))))))

;; TODO: Why not change the nonce when `request-update` is called and the email has been sent? Then I don't need the code in multiple places.
;; TODO: Only add a new email request if the person still is subscribed
;; TODO: Add button to unsubscribe both to the overview page, and eventually to the email.
;; TODO: After unsubscribing, delete the pending job. Currently that functionality may not yet be exposed in koyo/job.
(define (overview-page)
  (define subscribed?
    (get 'subscribed #f))
  (page
   (haml
    (.container
     (:h1 "Overview Page")
     (unless subscribed?
       (button
        (λ ()
          (with-study-transaction
            (put 'subscribed #t)
            (parameterize ([current-study-stack '(*root*)])
              (define nonce (generate-random-string))
              (define nonces (get/instance 'nonces (hasheqv)))
              (put/instance 'nonces (hash-set nonces (current-participant-id) nonce))
              (schedule-at
               (+minutes (now/moment) 1)
               (request-update
                (current-participant-id)
                nonce)))))
       "Subscribe to daily tracking reminders" #:to-step-id 'overview-page))
     (button void "Enter Sleep Data" #:to-step-id 'sleep-question)))))

(define sleep-tracker
  (make-study
   "sleep-tracker"
   #:requires '()
   #:provides '()
   (list
    (make-step 'overview-page overview-page)
    (make-step 'sleep-question sleep-question (λ () 'overview-page)))))
