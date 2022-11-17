#lang racket/base

(require racket/format
         racket/list
         racket/match
         "study.rkt")

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
      [(? number?)  e]

      [`(~a ,e) (~a (loop e))]

      [`(+ ,a ,b) (+ (loop a) (loop b))]
      [`(- ,a ,b) (- (loop a) (loop b))]
      [`(* ,a ,b) (* (loop a) (loop b))]
      [`(/ ,a ,b) (/ (loop a) (loop b))]

      [_ (error who "invalid expression: ~e" e)])))


;; randomization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-randomized-study)

(define (make-randomized-study input-steps)
  (define steps
    (shuffle
     (for/list ([(id step) (in-hash input-steps)])
       (make-step id step (λ ()
                            (begin0 next
                              (put 'steps (map step-id steps))))))))
  (make-study "randomized-study" steps))
