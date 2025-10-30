#lang conscript

(provide
 auction-14)

(require conscript/admin
         conscript/form0
         conscript/survey-tools
         racket/match)

(define roles '(participant-1 participant-2 participant-3))
(define n (length roles))

(defvar/instance group-roles)
(defvar/instance group-participants)
(defvar/instance bids)
(defvar role)

(define group-roles-box
  (case-lambda
     [() group-roles]
     [(v) (set! group-roles v)]))

(define group-participants-box
  (case-lambda
     [() group-participants]
     [(v) (set! group-participants v)]))

(define (set!/if-undefined l-box v)
  (with-study-transaction
    (when (undefined? (l-box))
      (l-box v))))

(define instructions
  @md*{# Instructions
 In this experiment you will participate in an auction. There are randomly assigned groups of @(~a n) bidders, a certificate is auctioned off, and each bidder receives a value for the certificate randomly drawn from the integer numbers E$ 10 to E$ 50. As before, each bidder knows his own value, but does not know the value of the other bidders.

 After every bidder received his value, the bidding process starts. **Simultaneously** and only once, all bidders are asked to **submit their bid** for the certificate. The auction winner is the bidder who submitted the highest bid (with random selection in case of a tie). The **price** will be equal to the **submitted bid of the auction winner.**

 Thus, the payoff of the auction winner from this experiment will be his value for the certificate minus the price he paid. The other bidders will have a payoff of zero.})

(defstep (init)
  (set!/if-undefined group-participants-box (hash))
  (set!/if-undefined group-roles-box (hash))
  (when (undefined? bids)
    (set! bids (hash)))
  (skip))

(defstep (intro)
  @md{@instructions

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

(defvar bid)
(defvar certificate-value)

(define (bidder-number i)
  (match i
    ['participant-1 "1"]
    ['participant-2 "2"]
    ['participant-3 "3"]
    [_ "Unknown participant"]))

(defstep (the-auction)
  #;(define current-group-roles
    (hash-ref group-roles (get-current-group)))
  (set! certificate-value (+ 10 (random 41)))
  (define-values (f on-submit)
    (form+submit
     [bid (ensure
           binding/number
           (required)
           (number-in-range 0 +inf.0))]))
  (define (render rw)
    @div{@rw["bid" @input-number{Please submit your bid:}]
         @|submit-button|})
  @md{# Auction
      You were randomly matched to a group of @(~a n) bidders.
      You are bidder number @(bidder-number role).
      Your randomly drawn value for the certificate is @(~a certificate-value) points.

      @form[f on-submit render]
      @toggleable-xexpr["Show/Hide Instructions"
                        instructions]})

(defstep (store-bids)
  (with-study-transaction
    ; Retrieve the current group participants
    #;(define current-group-ids (hash-ref group-participants (get-current-group) '()))
    ; Retrieve the existing guesses hash or initialize a new one
    (define bids-so-far
      (hash-ref bids (get-current-group) (hash)))
    ; Add the current participant's guess to the guesses hash
    (define new-bids
      (hash-set bids-so-far (current-participant-id) bid))
    (set! bids
          (hash-set bids
                    (get-current-group)
                    new-bids)))
  (skip))

(defstep (wait-for-bids)
  (if (>= (length (hash-values (hash-ref bids (get-current-group) '()))) n)
      (skip) ; Proceed to the next step if all bids are submitted
      @md{# Please Wait

          Waiting for all participants to submit their bids...

          @refresh-every[2]})) ;

(defstep (the-end)
  (define bids-list (hash-values (hash-ref bids (get-current-group))))
  (eprintf "HERE bids: ~a; bids-list: ~a~n~n" bids bids-list)

  (define highest-bid (apply max bids-list))
  (define winner-id
    (car
     (sort
      (filter
       (lambda (id)
         (equal?
          (hash-ref (hash-ref bids (get-current-group)) id)
          highest-bid))
       (hash-keys (hash-ref bids (get-current-group)))) <)))
  (define winner-role
    (hash-ref
     (hash-ref group-roles (get-current-group)) winner-id))

  (define payoff (if (equal? (current-participant-id) winner-id)
                     (- certificate-value highest-bid)
                     0))

  (put/identity 'score payoff)

  @md{# Auction Results

      ## Your Participation

      - You were bidder number @(bidder-number role).

      - You submitted a bid of **@(~a bid)** points.

      - Your randomly drawn value for the certificate was **@(~a certificate-value)**.

      ## Auction Outcome

      - The highest bid was **@(~a highest-bid)** points.
      - This bid was submitted by Bidder @(bidder-number winner-role).

      ## Your Payoff

      - Your final payoff is **@(~a payoff)** points.
      @toggleable-xexpr["Show/Hide Instructions" instructions]
  })


(defstudy auction-14/no-admin
  [init --> intro
        --> matchmake
        --> register-as-group-participant
        --> wait-for-group-ids
        --> assign-roles
        --> the-auction
        --> store-bids
        --> wait-for-bids
        --> the-end]
  [the-end --> the-end])

(defstep (admin)
  @md{# Admin})

(define auction-14
  (make-admin-study
   #:models `()
   #:admin admin
   auction-14/no-admin))
