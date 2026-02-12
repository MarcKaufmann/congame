#lang racket/base

(require koyo/database
         koyo/url
         racket/contract/base
         racket/lazy-require
         threading
         "study.rkt")

(provide
 (contract-out
  [send-push-notification
   (->* [id/c string?]
        [#:title (or/c #f string?)]
        void?)]))

(lazy-require
 [congame-web/components/push-notification
  ([send-push-notification web:send-push-notification])])

(define (send-push-notification
         #:title [title #f]
         #:url [url (make-study-url)]
         pid message)
  (define u (lookup-participant-user pid))
  (define data
    (~> (hasheq)
        (when~> url
          (hash-set 'url url))))
  (web:send-push-notification
   #:title title
   #:data data
   u message))

(define (make-study-url)
  (make-application-url "study" (current-study-instance-slug)))
