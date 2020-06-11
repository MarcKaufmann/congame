#lang racket/base

(require racket/contract
         xml
         "auth.rkt")

;; TODO:
;;  * A table that contains study medata: (name of the study, enslitment code, ...)
;;  * A table that holds participation information for every user: (user_id, study_id, study_position, enlisted_at)
;;  * An analogue of study_wide for `get' and `put': (user_id, study_id, study_stack, key, value, first_put_at, last_put_at)
;;    -- study stack should be a pg array
;;    -- key should just be text

(define (next)
  (void))

(define (done)
  (void))

;; To support cases where the step stores data as soon as it runs, we
;; may need some kind of (once ...) primitive or two variants of put:
;; one that only inserts data if a key isn't already set and one that
;; upserts.
(define (put k v)
  (void))

(define (get k [v (lambda ()
                    (error 'get "value not found for key ~.s" k))])
  (void))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button action label)
  (void))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NB: Avoid having to make a distinction between "normal" steps and
;; "action" steps if possible.

;; NB: Don't forget about forms!

(struct step (id handler transition)
  #:transparent)

(define step-id/c symbol?)
(define handler/c (-> xexpr?))
(define transition/c (-> step-id/c))

(define/contract (make-step id handler [transition next])
  (->* (step-id/c handler/c) (transition/c) step?)
  (step id handler transition))


;; study ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-study-stack
  (make-parameter null))

(struct study (requires provides steps)
  #:transparent)

(define/contract (make-study steps
                             #:requires [requires null]
                             #:provides [provides null])
  (->* ((non-empty-listof step?))
       (#:requires (listof symbol?)
        #:provides (listof symbol?))
       study?)
  (study requires provides steps))

(define/contract (run-study s)
  (-> study? any)
  (void))


;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (info)
  '(h1 "Welcome to the study."))

(define (give-consent)
  `(div
    (h1 "Do you consent?")
    ,(button
      (lambda ()
        (put 'consented? #t))
      "Yes")
    ,(button
      (lambda ()
        (put 'consented? #f))
      "No")))

(define consent-study
  (make-study
   #:provides '(consented?)
   (list
    (make-step 'info info)
    (make-step 'give-consent-1
               give-consent
               (lambda ()
                 (if (get 'consented?)
                     (next)
                     (done))))
    (make-step 'give-consent-2
               give-consent))))
