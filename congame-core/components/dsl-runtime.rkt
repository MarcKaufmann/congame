#lang racket/base

(require racket/format
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
