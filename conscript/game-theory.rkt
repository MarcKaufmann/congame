#lang conscript

(require data/monocle
         threading)

(provide
 chosen-action-profile
 outcomes
 outcome-matrix
 make-choice!
 payoff-matrix
 payoff-matrix/sym
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
  (define actions1
    (hash-ref game-form 'actions1))
  (define actions2
    (hash-ref game-form 'actions2))
  (define outcomes1
    (hash-ref game-form 'outcomes1))
  (define outcomes2
    (hash-ref game-form 'outcomes2))
  (define (outcome a b)
    (define ap (cons a b))
    (format "~a, ~a"
            (hash-ref outcomes1 ap)
            (hash-ref outcomes2 ap)))

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
        @(apply tr
          (cons
           @th{}
           (for/list ([a2 actions2])
             @th{@(~a a2)})))}
      @(apply tbody
          (for/list ([a1 actions1])
            (apply tr
              (cons
               @th{@(~a a1)}
               (for/list ([a2 actions2])
                 @td{@(outcome a1 a2)})))))}})

(define (payoff-matrix game-form
                       [utility1 identity]
                       [utility2 identity])
  (define actions1
    (hash-ref game-form 'actions1))
  (define actions2
    (hash-ref game-form 'actions2))
  (define (new-game-payoffs actions1 actions2 utility)
    (for*/hash ([a1 actions1]
                [a2 actions2])
      (values
       (cons a1 a2)
       (utility (cons a1 a2)))))
  (define new-game-form
    (~> (hash)
        (hash-set _ 'actions1 actions1)
        (hash-set _ 'actions2 actions2)
        (hash-set _ 'outcomes1
                  (new-game-payoffs actions1 actions2 utility1))
        (hash-set _ 'outcomes2
                  (new-game-payoffs actions1 actions2 utility2))))
  (outcome-matrix new-game-form))

(define (payoff-matrix/sym game-form [utility identity])
  (define (flipped-utility u)
    (lambda (a)
      (u (cons (cdr a) (car a)))))
  (payoff-matrix game-form utility (flipped-utility utility)))

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
