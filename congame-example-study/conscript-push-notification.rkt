#lang conscript

(require conscript/form0)

(provide
 conscript-push-notification)

(defvar title)
(defvar message)

(defstep (start)
  (define-values (f on-submit)
    (form+submit
     [title (ensure binding/text (required))]
     [message (ensure binding/text (required))]))
  (define (render rw)
    @md{@rw["title" @input-text{Title}]
        @rw["message" @textarea{Message}]
        @|submit-button|})
  @md{# Start

      @form[f on-submit render]})

(defstep (send-notification)
  @md{# Send

      Notification sent.

      @button{Continue}})

(defstep (done)
  @md{# Done

      You're done.})

(defstudy conscript-push-notification
  [start --> ,(λ ()
                (send-push-notification
                 #:title title
                 (current-participant-id)
                 message)
                (goto send-notification))]
  [send-notification --> done]
  [done --> done])
