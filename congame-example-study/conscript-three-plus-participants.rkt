#lang conscript

;; TODO: Update to work with matchmake.

(require conscript/form0
         conscript/survey-tools)

(provide
 threeplus)

; Number of people per group
(define n 3)
; Names of possible roles and their frequency
; '(a a b) means two participants get role 'a, one gets role 'b
(define roles '(a a b))

(defvar/instance group-roles)
(defvar/instance group-participants)
(defvar/instance choices-a)
(defvar role)

(define-var-box group-roles-box group-roles)
(define-var-box group-participants-box group-participants)

; TODO: Name doesn't reflect that this is meant for instance level only.
(define (set!/if-undefined l-box v)
  (with-study-transaction
    (when (undefined? (l-box))
      (l-box v))))

(defstep (intro)
  (set!/if-undefined group-participants-box (hash))
  (set!/if-undefined group-roles-box (hash))
  @md{# Three Plus

      @button{Continue}})

(defstep (waiter)
  @md{# Please Wait

      Please wait while the other participants join the queue...

      @refresh-every[5]})

(defstep matchmake
  (let ([matchmaker (make-matchmaker n)])
    (lambda ()
      (matchmaker waiter))))

(define (current-group-participants)
  (hash-ref group-participants (get-current-group) '()))

(defstep (register-as-group-participant)
  (with-study-transaction
    ; Get the ids of participants of this group that have already registered.
    ; Empty list if noone has registered yet.
    (define new-group-participants
      (cons (current-participant-id) (current-group-participants)))
    (set! group-participants
          (hash-set group-participants (get-current-group) new-group-participants)))
  (skip))

(defstep (wait-for-group-ids)
  (if (>= (length (current-group-participants)) n)
      (skip)
      @md{# Please Wait

          Please wait until your group has fully registered.

          @refresh-every[1]}))

(defstep (assign-roles)
  (with-study-transaction
    ; Unless we already have roles for the current group...
    (unless (hash-ref group-roles (get-current-group) #f)
      ; get participant ids
      (define pids
        (hash-ref group-participants (get-current-group)))
      ; create a hash (dict) where each pid is mapped to a random role
      (define assigned-roles
        (for/hash ([pid pids]
                   [r (shuffle roles)])
          (values pid r)))
      ; store this hash in group-roles under the current group
      (set! group-roles
            (hash-set group-roles (get-current-group) assigned-roles))))
  ; Set the role in a local variable for the current participant
  (set! role
        (hash-ref
         (hash-ref group-roles (get-current-group))
         (current-participant-id)))
  (skip))

(defstep (display-roles)
  (define current-group-roles
    (hash-ref group-roles (get-current-group)))

  @md{# Display Roles

      Your role is @(~a role)

      The full set of roles are:

      @`(ol
          ,@(for/list ([(k v) (in-hash current-group-roles)])
              (li (format "Participant ~a has role ~a" k v))))

      @button{Continue}
      })

(defvar choice-a)

(defstep (branch-for-a)
  @md{# Entered Branch A

      Choose some number:

      @form{
            @set![choice-a (input-number #:min 0 #:max 10)]
            @submit-button
            }})

(defstep (store-choice-a)
  (with-study-transaction
    ; One other player A might already have made their choice.
    ; Return the empty list otherwise.
    (define choices-a-so-far
      (hash-ref (if-undefined choices-a (hash)) (get-current-group) '()))
    (set! choices-a
          (hash-set
           (if-undefined choices-a (hash))
           (get-current-group)
           (cons choice-a choices-a-so-far))))
  (skip))

(defstep (branch-for-b)
  (define group-choices-a
    (hash-ref choices-a (get-current-group) #f))
  (cond [(= 2 (length group-choices-a))
         (skip)]

        [else
         @md{# Entered Branch B

             Wait until all player A's have made their choice.

             @refresh-every[2]}]))

(defstep (the-end)
  (define group-choices-a
    ; No longer checking for #f, since this should always have a value.
    (hash-ref choices-a (get-current-group)))
  ; This is arbitrary, assume each player A gets what they choose
  ; Player B gets the sum of what was chosen.
  (eprintf "group-choices-a: ~a; role: ~a; participant-id: ~a" group-choices-a role (current-participant-id))
  (define amount
    (if (equal? role 'a)
        choice-a
        (+ (first group-choices-a) (second group-choices-a))))

  @md{# The End

      You get: @(~a amount).})

(defstudy threeplus
  [intro --> matchmake
         --> register-as-group-participant
         --> wait-for-group-ids
         --> assign-roles
         --> display-roles
         --> ,(lambda ()
                (case role
                  [(a) 'branch-for-a]
                  [(b) 'branch-for-b]))]

  [branch-for-a --> store-choice-a --> the-end]
  [branch-for-b --> the-end]
  [the-end --> the-end])
