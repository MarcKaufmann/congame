#lang conscript/with-require

(require conscript/admin
         conscript/form0
         racket/match
         "abc.rkt"
         "cde.rkt"
         "fee-sig.rkt"
         "study-sig.rkt")

(provide
 random-designs
 random-designs-with-admin)

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
  (set! selected-design (car (random-ref designs)))
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

(define random-designs-with-admin
  (make-admin-study
   #:models `((simple . ,(λ (id _bot)
                           (match id
                             ['(*root* abc ask-payment) (bot:autofill 'cheap)]
                             ['(*root* the-end) (bot:completer)]
                             [_ (bot:continuer)]))))
   random-designs))
