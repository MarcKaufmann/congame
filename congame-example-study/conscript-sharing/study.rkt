#lang conscript/with-require

(require racket/unit
         "result-sig.rkt"
         "sub-a.rkt"
         "sub-b.rkt"
         "sub-sig.rkt")

(provide
 conscript-sharing-study)

(with-namespace xyz.trichotomy.congame.conscript-sharing.parent
  (defvar* a-treatment)
  (defvar* b-treatment)
  (defvar* a-result)
  (defvar* b-result))

(define sub-study-a
  (let ([store-treatment (lambda (t) (set! a-treatment t))]
        [store-result (lambda (r) (set! a-result r))])
    (define-values/invoke-unit sub-a@
      (import result^)
      (export sub^))
    sub-study))

(define sub-study-b
  (let ([store-treatment (lambda (t) (set! b-treatment t))]
        [store-result (lambda (r) (set! b-result r))])
    (define-values/invoke-unit sub-b@
      (import result^)
      (export sub^))
    sub-study))

(defstep (start)
  @md{# Start

      @button{Continue}})

(defstep (end)
  @md{# Done

      You've reached the end.})

(defstudy conscript-sharing-study
  [start --> sub-study-a --> sub-study-b --> end]
  [end --> end])
