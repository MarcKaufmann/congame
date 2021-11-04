#lang racket/base

(require deta
         gregor)

(provide
 (schema-out message))

(define-schema message
  #:table "messages"
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [sender string/f]
   [subject string/f]
   [data binary/f]
   [(received-at (now/moment)) datetime-tz/f]))
