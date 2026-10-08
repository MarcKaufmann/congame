#lang conscript/with-require

;; Line random-deisngs.rkt, but with an example of "bad" randomness
;; that will not work properly with study->bot.

(require conscript/admin
         conscript/form0
         racket/match
         "abc.rkt"
         "cde.rkt"
         "fee-sig.rkt"
         "study-sig.rkt")

(provide
 random-designs-bad
 random-designs-bad-with-admin)

(with-namespace xyz.trichotomy.many-designs.random-bad
  (defvar* fee)
  (defvar* selected-design))

(define designs
  (list (cons 'abc abc-study@)
        (cons 'cde cde-study@)))

(defstep (the-beginning)
  (set! fee 42)
  @md{# The Beginning

      @button{Continue}})

(defstep/study run-design
  #:study (λ ()
            ;; Here is the badness compared to random-designs.rkt. In the
            ;; original module, the random study is selected in a static step,
            ;; and that selection persists into this dynamic substudy. So, every
            ;; time the dynamic substudy runs, the same design@ is selected for
            ;; any given participant. In this example, however, we select a
            ;; random design@ every time, so the bot participant and the study
            ;; participant can drift, making the bot fail to look up the model
            ;; for a step _some of the time_.
            (define design@ (cdr (random-ref designs)))
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

(defstudy random-designs-bad
  [the-beginning --> run-design --> the-end]
  [the-end --> the-end])

(define random-designs-bad-with-admin
  (make-admin-study
   #:models `((simple . ,(λ (id _bot)
                           (match id
                             ['(*root* abc ask-payment) (bot:autofill 'cheap)]
                             ['(*root* the-end) (bot:completer)]
                             [_ (bot:continuer)]))))
   random-designs-bad))
