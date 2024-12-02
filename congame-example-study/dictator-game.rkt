#lang conscript

(require conscript/survey-tools
         data/monocle)

(provide
 dictator-game)

(defvar role)
(defvar/instance groups)
(defvar/instance group-vars)
(defvar/instance roles)
(defvar choice)
(defvar/instance choices)
(defvar other-id)

(defstep (init)
  (set! group-vars (hash))
  (set! groups (hash))
  (set! choices (hash))
  (skip))

(defstep (waiter)
  @md{# Please Wait

 Please wait while another participant joins the queue.

 @refresh-every[5]})

(defstep matchmake
  (let ([matchmaker (make-matchmaker 2)])
    (lambda ()
      (matchmaker waiter))))

(define (get-group-vars)
  (hash-ref group-vars (get-current-group) #f))

(define (get-group-members)
  (hash-ref groups (get-current-group) '()))

(defstep (store-self-id)
  (with-study-transaction
    (parameterize ([current-hash-maker hash])
      (set! groups
            (lens-update
             (&opt-hash-ref* (get-current-group))
             groups
             (lambda (lop)
               (cons (current-participant-id) (if lop lop '())))))))
  (skip))

(defstep (assign-roles)
  (set! other-id
        (findf
         (lambda (x) (not (equal? x (current-participant-id))))
         (get-group-members)))
  (define gid (get-current-group))
  (define self-id (current-participant-id))
  (eprintf "participant ~a about to enter, own role: ~a~n~n" self-id (get-own 'role))
  (with-study-transaction
    (unless (get-own 'role)
      (eprintf "participant ~a entered study transaction~n~n" self-id )
      (define roles
        (shuffle '(dictator receiver)))
      (parameterize ([current-hash-maker hash])
        (define group-vars/role1
          ((&opt-hash-ref* gid self-id 'role) group-vars (first roles)))
        (define group-vars/both-roles
          ((&opt-hash-ref* gid other-id 'role) group-vars/role1 (second roles)))
        (set! group-vars group-vars/both-roles))))
  (when (if-undefined role #t)
    (set! role (get-own 'role)))
  (skip))

(define (get-own k)
  ((&opt-hash-ref* (get-current-group) (current-participant-id) k)
   group-vars))

(defstep (display-role)
    @md{# Your Role

        Your role is @(~a (get-own 'role)).

        @button{Next}})

(defstep (dictator)
  @md{# Dictator

      @form{
            @(set! choice (input-number #:min 0 #:max 10 "How much do you take for yourself?"))
            @submit-button}})

(defstep (store-choice)
  (set! choices
        (hash-set choices (get-current-group) choice))
  (skip))

(defstep (the-dictator-end)
  @md{# The end

      You chose to take @(~euro choice).

      The end.})

(defstep (receiver)
  (define taken-amount
    (hash-ref choices (get-current-group) #f))

  (cond [taken-amount
         @md{# The end

             You receiveed @(~euro (- 10 taken-amount)).}]
        [else
         @md{# The choice was not yet made

             Please wait.}]))

(defstep (wait-for-other-id)
  (if (= 2 (length (hash-ref groups (get-current-group) '())))
      (skip)
      @md{# Wait

          Please wait a bit.

          @refresh-every[1]}))

(defstudy dictator-game
  [init --> matchmake
        --> store-self-id
        --> wait-for-other-id
        --> assign-roles
        --> display-role
        --> ,(lambda ()
               role)]
  [dictator --> store-choice --> the-dictator-end --> the-dictator-end]
  [receiver --> receiver])
