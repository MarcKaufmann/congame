#lang racket/base

(require buid
         deta
         gregor
         koyo/random
         racket/string)

(provide
 (schema-out shadow))

(define-schema shadow
  #:table "user_shadows"
  ([user-id id/f]
   [server-id id/f]
   [instance-id id/f]
   [(display-name (generate-random-display-name)) string/f #:contract non-empty-string? #:wrapper string-downcase]
   [(api-key (generate-api-key)) string/f]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])

  #:pre-persist-hook
  (lambda (u)
    (set-shadow-updated-at u (now/moment))))

(define (generate-random-display-name)
  (format "u.~a" (buid)))

(define (generate-api-key)
  (generate-random-string 48))
