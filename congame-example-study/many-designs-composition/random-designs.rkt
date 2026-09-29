#lang conscript/with-require

(require "abc.rkt"
         "cde.rkt"
         "fee-sig.rkt"
         "study-sig.rkt")

(provide
 random-designs)

(with-namespace xyz.trichotomy.many-designs.random
  (defvar* fee)
  (defvar* selected-design))

(define designs
  (list (cons 'abc abc-study@)
        (cons 'cde cde-study@)))

(defstep (the-beginning)
  (set! fee 42)
  @md{# The Beginning

      @button{Continue}})

(defstep (pick-design)
  (set! selected-design (car (list-ref designs (random (length designs)))))
  (skip))

(defstep/study run-design
  #:study (λ ()
            (define design@ (cdr (assq selected-design designs)))
            (define (get-fee) fee)
            (define-values/invoke-unit design@
              (import fee^)
              (export study^))
            (defstudy design-study
              [{,selected-design study} --> ,(λ () done)])
            design-study))

(defstep (the-end)
  @md{# The End

      Your design was @~a[selected-design].})

(defstudy random-designs
  [the-beginning --> pick-design --> run-design --> the-end]
  [the-end --> the-end])
