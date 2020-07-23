#lang racket/base

(require (for-syntax racket/base)
         koyo/continuation
         koyo/haml
         (except-in forms form)
         racket/contract
         racket/match
         racket/stxparam
         web-server/servlet
         xml)

(provide
 next
 done
 put
 get
 button
 form
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

(define-syntax-parameter continue
  (lambda (stx)
    (raise-syntax-error 'continue "can only be used inside with-continue-parameterization" stx)))

(define-syntax-rule (with-continue-parameterization e ...)
  ;; Capture return here so that any embedded (via embed/url) lambda can close over it.
  (let ([return (current-return)])
    (syntax-parameterize ([continue (syntax-rules ()
                                      [(_)     (return 'continue)]
                                      [(_ res) (return res)])])
      e ...)))

(define current-embed/url
  (make-parameter 'no-embed/url))

(define current-request
  (make-parameter 'no-request))

(define current-return
  (make-parameter 'no-return))

(define (button action label)
  (with-continue-parameterization
    (haml
     (:a
      ([:href
        ((current-embed/url)
         (lambda (_req)
           (action)
           (continue)))])
      label))))

(define (form f action render)
  (define the-study (current-study))
  (define the-step (current-step))
  (with-continue-parameterization
    (match (form-run f (current-request))
      [(list 'passed res _)
       (action res)
       (continue)]

      [(list _ _ rw)
       (haml
        (:form
         ([:action ((current-embed/url)
                    (lambda (req)
                      ;; TODO: Signal to the outer run-step that we are done.
                      ;; BUG: We only want this to re-render the page,
                      ;; but the study should control moving forward.
                      ;; NB: If we use a loop, then we have to deal
                      ;; with passing a rendered form into tell-name
                      ;; somehow.
                      (run-step req the-study the-step)))]
          [:method "POST"])
         (render rw)))])))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-logger study)

(define current-study-stack
  (make-parameter null))

(define current-study
  (make-parameter 'no-study))

(define current-step
  (make-parameter 'no-step))

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

;; Re. preconditions: if we bake them into steps/studies, there doesn't need
;; to be a separate stack of preconditions, because when we resume,
;; we'll run the precondition for each step (or possibly study).
(define/contract (run-study s [req (current-request)])
  (->* (study?) (request?) any)
  (parameterize ([current-study-stack (cons s (current-study-stack))])
    (define the-step (study-next-step s))
    (run-step req s the-step)))

(define (run-step req s the-step)
  (log-study-debug "running step: ~.s" the-step)
  (define res
    (call-with-current-continuation
     (lambda (return)
       (send/suspend/dispatch/protect
        (lambda (embed/url)
          (parameterize ([current-embed/url embed/url]
                         [current-request req]
                         [current-return return]
                         [current-study s]
                         [current-step the-step])
            (response/xexpr ((step-handler the-step)))))))
     servlet-prompt))

  (log-study-debug "step ~.s returned ~.s" the-step res)
  (cond
    [(response? res)
     (send/back res)]

    #;
    [(finish? res)
     (finish-res res)]

    [else
     (define new-req (redirect/get/forget/protect))
     (define next-step
       (match ((step-transition the-step))
         [(? done?) #f]
         [(? next?) (study-find-next-step s (step-id the-step))]
         [next-step-id (study-find-step s next-step-id)]))

     (cond
       [next-step => (lambda (the-next-step)
                       (run-step new-req s the-next-step))]
       [else
        (define study-res
          (for/hasheq ([id (in-list (study-provides s))])
            (values id (get id (lambda ()
                                 (error 'run-study "study did not 'put' provided variable: ~s" id))))))

        (log-study-debug "study result: ~.s" study-res)
        study-res])]))

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
