#lang racket/base

(require "../components/study.rkt"
         "../components/study-example.rkt"
         "../components/template.rkt")

(provide
 study-page)

(define (study-page req)
  (define res (run-study consent-study req))
  (printf "result: ~a~n" res)
  (flush-output)
  (page
   '(p "Yer done")))
