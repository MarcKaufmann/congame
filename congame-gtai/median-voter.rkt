#lang conscript

(require conscript/admin
         conscript/game-theory
         conscript/survey-tools
         data/monocle
         racket/list
         racket/unit)

(provide
 median-voter-lecture/admin)

(with-namespace xyz.trichotomy.congame.median-voter
  (defvar* score)
  (defvar*/instance game-results)
  (defvar*/instance choices)
  (defvar*/instance choices/rounds))

(defbox choices)
(defbox choices/rounds)
(define-values/invoke-unit game-theory@
  (import game-theory-vars^)
  (export game-theory^))

(defstep (instructions)
  @md{# Vote, vote, vote!

      Consider the following game: you are one of two candidates in an election. Both of you can choose where to position yourself on the political spectrum ranging from 1 ("Strongly Left") to 5 ("Strongly Right").

      After you both made your decision where to position yourself, the voters vote for the candidate that is closest to their own position, with 20% of voters are at each position. If both candidates are equally close to the voters, then the voters split exactly 50-50.

      For example, the 20% of voters at position 1 will vote for the candidate that is closest to 1. If both candidates are equidistant, then the voters split 50-50.

      Assume for now that the goal of the candidates is to maximize the share of the vote. That is, your score from this game is proportional to the vote share you get.

      You will now play the above game after being paired with another student at random.

      @button{Continue}})

; TODO: How much of this can we refactor into survey-tools or similar?
; E.g.: (define matchmake2 (let ...)) and then use matchmake2 directly. Any issue?
(defstep (match-waiter)
  @md{# Please Wait

      Please wait while another participant joins the queue.

      @refresh-every[5]})

(define matchmake
  (let ([matchmaker (make-matchmaker 2)])
    (lambda ()
      (matchmaker match-waiter))))

(defvar choice)

(defstep (make-choice)
  @md{# Choose your Position

      @form{
            @set![choice @input-number[#:min 1 #:max 5]{
              @md*{Where do you position yourself? (Range: 1 = "Strongly Left", 5 = "Strongly Right")}
            }]
            @submit-button
      }})

(defstep (store-choice!)
  (make-choice! choice)
  (skip))

; Copy paste from grade-game, refactor to game-theory?
(defstep (wait-for-other-player)
  (if (= (hash-count ((&hash-ref* (get-current-group)) choices)) 1)
      @md{# Please Wait

          Please wait for the other participant to make their choice...

          @refresh-every[5]}
      (skip)))

(define (own-vote-share ap)
  ; Vote share measured in percent
  (define p1 (car ap))
  (define p2 (cdr ap))
  (for/fold ([vs 0])
            ([i (range 1 6)])
    (define diff1 (abs (- p1 i)))
    (define diff2 (abs (- p2 i)))
    (cond [(< diff1 diff2)
           (+ vs 20)]

          [(= diff1 diff2)
           (+ vs 10)]

          [else
           vs])))

(defstep (display-result)
  (define actions (chosen-action-profile))
  (define own-share
    (own-vote-share actions))
  (define other-share
    (- 100 own-share))

  ; TODO: Could this be called twice by submitting twice quickly or some such? Or somehow repeating the request?
  (define (update-score!)
    (set! score
          (+ score own-share)))

  (define result
    (hash 'actions actions
          'shares (cons own-share other-share)))

  ; NOTE: this overwrites the other person's data, but this
  ; only changes the order which doesn't matter for current
  ; purposes.
  (define (store-result!)
    (with-study-transaction
      (set! game-results
            (hash-set game-results
                      (get-current-group)
                      result))))

  (define (update-score-and-store-result!)
    (update-score!)
    (store-result!))

  @md{# Result

      - Your position: @(~a (car actions))
      - The other candidate's position: @(~a (cdr actions))

      ## Vote shares

      - Your vote share: @(~a own-share)
      - Other's vote share: @(~a (- 100 own-share))

      @button[update-score-and-store-result!]{Continue}})

(defstep (the-end)
  @md{# The End

      Thank you for participating.

      Your total score is @(~a score)})

(defstudy median-voter-game
  [matchmake
   --> make-choice
   --> store-choice!
   --> wait-for-other-player
   --> display-result
   --> ,(lambda ()
          (reset-current-group)
          done)])

(defstep (init)
  (when (undefined? choices)
    (set! choices (hash)))
  (when (undefined? score)
    (set! score 0))
  (when (undefined? game-results)
    (set! game-results (hash)))
  (skip))

(defstep (put-score-identity)
  (put/identity 'median-voter-score score)
  (skip))

(defstudy median-voter-lecture
  [init
   --> instructions
   --> median-voter-game
   --> put-score-identity
   --> the-end]

  [the-end --> the-end])

;; ADMIN

(defstep (admin)
  (with-study-transaction
    (when (undefined? game-results)
      (set! game-results (hash))))

  (define action-count
    (for/fold ([hist (hash)])
              ([h (hash-values game-results)])
      (define ap
        (hash-ref h 'actions))
      (define a1 (car ap))
      (define a2 (cdr ap))
      (hash-update
       (hash-update hist a1 add1 0)
       a2 add1 0)))

  @md{# Admin

      ## Results

      @`(ul
         ,@(for/list ([i (range 1 6)])
             (li (format "~a occurs ~a times"
                         i (hash-ref action-count i 0)))))
      })

(define median-voter-lecture/admin
  (make-admin-study
   #:models `()
   #:admin admin
   median-voter-lecture))
