#lang conscript

(require gregor
         koyo/job)

(provide
 send-email-study)

(defstep (start)
  (schedule-at
   (+minutes (now/moment) 5)
   (send-email "Hi" "Thanks for joining the study."))
  @md{# Start

      @button{Continue}})

(defstep (end)
  @md{# You're done

      Check your email.})

(defstudy send-email-study
  [start --> end]
  [end --> end])
