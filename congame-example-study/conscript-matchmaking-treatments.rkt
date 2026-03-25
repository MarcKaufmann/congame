#lang conscript

(require conscript/matchmaking
         conscript/survey-tools)

(provide
 conscript-matchmaking-treatments)

(defvar type) ; (or/c 'control 'treatment)
(defvar/instance group-id->type) ; (hash/c group-id type/c)

(define (get-group-types)
  (if-undefined group-id->type (hash)))

(define (get-group-type gid)
  (hash-ref (get-group-types) gid #f))

(define (set-group-type! gid t)
  (set! group-id->type (hash-set (get-group-types) gid t)))

(defstep (start)
  ;; NOTE: We're being lazy here. This is not balanced and refreshing
  ;; the step will reassign the type.
  (set! type (random-ref '(control treatment)))
  @md{# Welcome

      Welcome to the study!

      You are in the @~a[type] group.

      @button{Continue}})

(defstep (waiter)
  @md{# Please Wait

      Please wait while other participants join your group...

      @refresh-every[5]})

(define matchmaker
  (make-matchmaker
   #;group-size 2
   #;group-ok?
   (lambda (group-id)
     (equal? type (get-group-type group-id)))
   #:on-group-create
   (lambda (group-id)
     (set-group-type! group-id type))))

(defstep (matchmake)
  (matchmaker waiter))

(defstep (show-type)
  @md{# Done

      You were in the @~a[type] group.})

(defstudy conscript-matchmaking-treatments
  [start --> matchmake --> show-type --> ,(λ () done)])
