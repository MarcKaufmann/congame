#lang racket/base

(require component
         koyo/database
         koyo/job
         racket/contract/base
         racket/lazy-require
         (submod "study.rkt" private)
         "study.rkt")

(provide
 (contract-out
  [send-email
   (-> string? string? id/c)]))

(lazy-require
 [congame-web/components/mail
  (mailer-send-generic-participant-email)])

(define-job (conscript-send-participant-email pid subject content)
  (define db (system-ref 'db))
  (define p (lookup-study-participant/by-id db pid))
  (call-with-study-manager
   (make-study-manager
    #:database db
    #:participant p)
   (lambda ()
     (define m (system-ref 'mailer))
     (define u (lookup-participant-user pid))
     (mailer-send-generic-participant-email m u subject content))))

(define (send-email subject content)
  (define mgr (current-study-manager))
  (define p (study-manager-participant mgr))
  (conscript-send-participant-email
   (study-participant-id p)
   subject
   content))
