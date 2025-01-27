#lang at-exp racket/unit

(require conscript/base
         data/monocle
         threading
         "game-theory-sig.rkt"
         "game-theory-vars-sig.rkt")

(import game-theory-vars^)
(export game-theory^)

(define (&my-choice)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     (current-participant-id))))

(define (make-choice! choice)
  (with-study-transaction
    (set!-choices ((&my-choice) (get-choices) choice))))

(define (&my-choice/rounds r)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     r
     (current-participant-id))))

(define (make-choice/rounds! choice r)
  (with-study-transaction
    (set!-choices/rounds ((&my-choice/rounds r) (get-choices/rounds) choice))))

(define (get-own-choice)
  ((&my-choice) (get-choices)))

(define (get-own-choice/rounds r)
  ((&my-choice/rounds r) (get-choices/rounds)))

(define (get-other-choice)
  (define-values (_my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash (hash-ref (get-choices) (get-current-group)))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  other-choice)

(define (&group-choices)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref* (get-current-group))))

(define (&group-choices/rounds r)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref* (get-current-group) r)))

; This assumes that there are two people, but doesn't check for it. Will return some random other participant as 'other' otherwise.
(define (get-other-choice/rounds r)
  (define-values (_my-choice other-choice)
    (for/fold ([my-choice #f]
               [other-choice #f])
              ([(k v) (in-hash ((&group-choices/rounds r) (get-choices/rounds)))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values my-choice v))))
  other-choice)

(define (chosen-action-profile)
  (define-values (own-choice other-choice)
    (for/fold ([own-choice #f]
               [other-choice #f])
              ([(k v) (in-hash ((&opt-hash-ref* (get-current-group)) (get-choices)))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values own-choice v))))
  (cons own-choice other-choice))
