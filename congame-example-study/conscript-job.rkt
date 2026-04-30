#lang conscript

(require gregor)

(provide
 conscript-job-study)

(with-namespace com.totalinsightmanagement.conscript
  (defvar* x))

(define-job (send-an-email #:subject subject #:body [body ""])
  (eprintf "SUBJECT: ~s BODY: ~s x: ~s~n" subject body x))

(defstep (start)
  (set! x 'start)
  ;; Scoped to this participant.
  (schedule-at
   (+minutes (now/moment) 1)
   (send-an-email #:subject "Hello"))
  @md{# Hi

      @button{Continue}})

(defstep (end)
  (set! x 'end)
  @md{# Done})

(defstudy conscript-job-study
  [start --> end --> ,(λ () done)])
