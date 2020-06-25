#lang racket/base

(require koyo/continuation
         koyo/haml
         racket/contract
         racket/match
         web-server/http
         web-server/servlet
         xml
         "auth.rkt")

(provide
 next
 done
 put
 get
 button
 make-step
 make-study
 run-study)

;; TODO:
;;  * A table that contains study metadata: (name of the study, enlistment code, ...)
;;  * A table that holds participation information for every user: (user_id, study_id, study_position, enlisted_at)
;;  * An analogue of study_wide for `get' and `put': (user_id, study_id, study_stack, key, value, first_put_at, last_put_at)
;;    -- study stack should be a pg array
;;    -- key should just be text

(define-values (next next?)
  (let ()
    (struct next () #:transparent)
    (values (next) next?)))

(define-values (done done?)
  (let ()
    (struct done () #:transparent)
    (values (done) done?)))


(define *storage* (make-hash))

(struct storage-key (stack k)
  #:transparent)

;; To support cases where the step stores data as soon as it runs, we
;; may need some kind of (once ...) primitive or two variants of put:
;; one that only inserts data if a key isn't already set and one that
;; upserts.
(define (put k v)
  (hash-set! *storage* (storage-key (current-study-stack) k) v))

(define (get k [default (lambda ()
                          (error 'get "value not found for key ~.s" k))])
  (hash-ref *storage* (storage-key (current-study-stack) k) default))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-embed/url
  (make-parameter #f))

(define (button action label)
  (haml
   (:a
    ([:href ((current-embed/url)
             (lambda (_req)
               (action)
               ;; Because we run the action first, if it raises an
               ;; exception then the user will be able to refresh the
               ;; page and the action will run again.  This seems
               ;; mostly desirable in the face of one-off
               ;; errors/problems, but there may be cases when it's
               ;; not.  In those cases, we might have a separate sort
               ;; of button that reverses these two calls.
               (redirect/get/forget/protect)
               ;; The protected variants of embed/url, unlike their
               ;; web-server counterparts, require the embedded
               ;; function to be a handler (that is, it must produce a
               ;; response value) so that middleware may be applied to
               ;; it.  Because we don't care about the return value
               ;; here, we simply return an artificial response.
               ;; Middleware will be applied to it but it will have no
               ;; effect.  It's conceivable that we might add a
               ;; middleware that has side-effects, in which case this
               ;; may break so that's something to watch out for.
               (response/xexpr '(p "ignored"))))])
    label)))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NB: Avoid having to make a distinction between "normal" steps and
;; "action" steps if possible.

;; NB: Don't forget about forms!

;; Maybe embed preconditions into steps rather than attempting to wrap them.
(struct step (id handler transition)
  #:transparent)

(define step-id/c symbol?)
(define handler/c (-> xexpr?))
(define transition/c (-> (or/c done? next? step-id/c)))

(define/contract (make-step id handler [transition (lambda () next)])
  (->* (step-id/c handler/c) (transition/c) step?)
  (step id handler transition))


;; study ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-study-stack
  (make-parameter null))

(define-logger study)

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
  (parameterize ([current-study-stack (cons s (current-study-stack))])
    (let loop ([current-step (study-next-step s)])
      (log-study-debug "current-step: ~s" current-step)
      (send/suspend/dispatch/protect
       (lambda (embed/url)
         (parameterize ([current-embed/url embed/url])
           (response/xexpr
            ((step-handler current-step))))))

      (define next-step
        (match ((step-transition current-step))
          [(? done?) #f]
          [(? next?) (study-find-next-step s (step-id current-step))]
          [next-step-id (study-find-step s next-step-id)]))

      (cond
        [next-step => loop]
        [else
         (for/hasheq ([id (in-list (study-provides s))])
           (values id (get id (lambda ()
                                (error 'run-study "study did not 'put' provided variable: ~s" id)))))]))))

(define (study-next-step s)
  (car (study-steps s)))

(define (study-find-next-step s id)
  (for/fold ([previous #f]
             [next-step #f]
             #:result next-step)
            ([a-step (in-list (study-steps s))]
             #:unless next-step)
    (if (and previous (eq? (step-id previous) id))
        (values previous a-step)
        (values a-step #f))))

(define (study-find-step s id)
  (for/first ([a-step (in-list (study-steps s))]
              #:when (eq? (step-id a-step) id))
    a-step))
