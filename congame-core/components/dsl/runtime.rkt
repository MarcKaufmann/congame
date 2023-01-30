#lang racket/base

(require racket/format
         racket/list
         racket/match
         "../study.rkt")

(provide
 make-put-all-keywords
 interpret-basic-expr)

(define (make-put-all-keywords [action void])
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kw (in-list kws)]
           [arg (in-list kw-args)])
       (put (string->symbol (keyword->string kw)) arg))
     (action))))

(define (interpret-basic-expr who e)
  (let loop ([e e])
    (match e
      [`(quote ,e) e]

      [(? symbol?) e]
      [(? number?) e]

      [`(~a ,e) (~a (loop e))]

      [`(+ ,a ,b) (+ (loop a) (loop b))]
      [`(- ,a ,b) (- (loop a) (loop b))]
      [`(* ,a ,b) (* (loop a) (loop b))]
      [`(/ ,a ,b) (/ (loop a) (loop b))]

      [`(get ,id) (get (loop id))]

      [_ (error who "invalid expression: ~e" e)])))


;; randomization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-randomized-study)

;; NOTE: Someone could provide a step named
;; `synthetic-randomizer-step`, in which case it will error.
(define (make-randomized-study steps)
  (define (randomize)
    (define shuffled-steps (shuffle (hash-keys steps)))
    (put 'steps shuffled-steps)
    (put 'remaining-steps shuffled-steps)
    (skip))
  (make-study
   "randomized-study"
   (cons
    (make-step 'synthetic-randomizer-step randomize randomizer-transition)
    (for/list ([(id step) (in-hash steps)])
      (make-step id step randomizer-transition)))))

(define (randomizer-transition)
  (let ([steps (get 'remaining-steps)])
    (cond
      [(null? steps)
       done]
      [else
       (put 'remaining-steps (cdr steps))
       (car steps)])))
