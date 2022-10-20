#lang racket/base

(require "study.rkt")

(provide
 put-all-keywords)

(define put-all-keywords
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kw (in-list kws)]
           [arg (in-list kw-args)])
       (put (string->symbol (keyword->string kw)) arg)))))
