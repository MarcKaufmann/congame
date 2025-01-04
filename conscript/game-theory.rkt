#lang conscript

(require data/monocle
         threading)

(provide
 chosen-action-profile
 outcomes
 outcome-matrix
 make-choice!
 payoff-matrix
 choices)

(with-namespace xyz.trichotomy.congame.grade-game
  (defvar*/instance choices))

; Helper for getting and setting choices
; TODO: refactor for general 2x2 games

(define (&my-choice)
  (parameterize ([current-hash-maker hash])
    (&opt-hash-ref*
     (get-current-group)
     (current-participant-id))))

(define (make-choice! choice)
  (with-study-transaction
    (set! choices ((&my-choice) choices choice))))

; Helper functions to display games
; TODO: Refactor for general 2x2 games

(define (outcome-matrix game-form)
  (define actions
    (hash-ref game-form 'actions))
  (define a1 (first actions))
  (define a2 (second actions))
  (define (outcome a b)
    (define r
      (hash-ref game-form (cons a b)))
    (format "~a, ~a" (car r) (cdr r)))

  ; TODO: Refactor CSS into global(ish) module?
  @html*{
    @style{
      table.outcome-matrix {
        border-collapse: collapse;
        text-align: center;
      }
      .outcome-matrix thead th {
        border: none;
        padding: 1rem 2rem;
        font-weight: bold;
      }
      .outcome-matrix tbody th {
        text-align: left;
        padding: 1rem 2rem;
      }
      .outcome-matrix td {
        border: 1px solid #000;
        padding: 8px 16px;
        font-size: 1rem;
        width: 6rem;
      }
    }

    @table[#:class "outcome-matrix"]{
      @thead{
        @tr{
          @th{} @th{@(~a a1)} @th{@(~a a2)}}}
      @tbody{
        @tr{
          @th{@(~a a1)} @td{@(outcome a1 a1)} @td{@(outcome a1 a2)}}
        @tr{
          @th{@(~a a2)} @td{@(outcome a2 a1)} @td{@(outcome a2 a2)}}}}})

(define (payoff-matrix game-form [utility identity])
  (define actions
    (hash-ref game-form 'actions))
  (define new-game-form
    (~> (for*/hash ([a1 actions]
                    [a2 actions])
          (values
           (cons a1 a2)
           (cons (utility (cons a1 a2))
                 (utility (cons a2 a1)))))
        (hash-set _ 'actions actions)))
  (outcome-matrix new-game-form))

(define (chosen-action-profile)
  (define-values (own-choice other-choice)
    (for/fold ([own-choice #f]
               [other-choice #f])
              ([(k v) (in-hash ((&opt-hash-ref* (get-current-group)) choices))])
      (if (equal? k (current-participant-id))
          (values v other-choice)
          (values own-choice v))))
  (cons own-choice other-choice))

(define (outcomes game-form action-profile)
  (hash-ref game-form action-profile))
