#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header)
         koyo/continuation
         koyo/haml
         (except-in forms form)
         racket/contract
         racket/match
         racket/stxparam
         syntax/parse/define
         web-server/servlet
         xml)

(provide
 next
 done
 put
 get
 button
 form
 skip
 make-step
 make-step/study
 wrap-sub-study
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

(define (put k v)
  (hash-set! *storage* (storage-key (current-study-stack) k) v))

(define (get k [default (lambda ()
                          (error 'get "value not found for key ~.s" k))])
  (hash-ref *storage* (storage-key (current-study-stack) k) default))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-embed/url
  (make-parameter 'no-embed/url))

(define current-request
  (make-parameter 'no-request))

(define current-return
  (make-parameter 'no-return))

(define-syntax-rule (define-widget-stxparams id ...)
  (begin
    (define-syntax-parameter id
      (lambda (stx)
        (raise-syntax-error #f "can only be used inside with-widget-parameterization" stx)))
    ...))

(define-widget-stxparams
  embed
  this-request
  this-step
  continue)

(define-syntax-rule (with-widget-parameterization e ...)
  ;; Capture return here so that any embedded (via embed/url) lambda can close over it.
  (let ([embed/url (current-embed/url)]
        [the-request (current-request)]
        [the-step (current-step)]
        [return (current-return)])
    (syntax-parameterize ([embed
                           (syntax-parser
                             [(_ f:expr)
                              #'(embed/url
                                 (lambda (req)
                                   (parameterize ([current-embed/url embed/url]
                                                  [current-request req]
                                                  [current-return return]
                                                  [current-step the-step])
                                     (f req))))])]

                          [continue
                           (syntax-parser
                             [(_)
                              #'(return 'continue)]

                             [(_ to:id)
                              #'(return (cons 'to-step to))])]

                          [this-request
                           (lambda (stx)
                             (syntax/loc stx the-request))]

                          [this-step
                           (lambda (stx)
                             (syntax/loc stx the-step))])
      e ...)))

(define-syntax-parser define/widget
  [(_ head:function-header body ...+)
   #'(define head
       (with-widget-parameterization
         body ...))])

(define/widget (button action label)
  (haml
   (:a
    ([:href
      (embed
       (lambda (_req)
         (action)
         (continue)))])
    label)))

(define/widget (form f action render)
  (match (form-run f this-request)
    [(list 'passed res _)
     (action res)
     (continue)]

    [(list _ _ rw)
     (haml
      (:form
       ([:action (embed
                  (lambda (_req)
                    (response/xexpr
                     ((step-handler this-step)))))]
        [:method "POST"])
       (render rw)))]))

(define/widget (skip [to-step-id #f])
  (if to-step-id
      (continue to-step-id)
      (continue)))


;; step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct step (id handler transition)
  #:transparent)

(struct step/study step (study)
  #:transparent)

(struct study (requires provides steps)
  #:transparent)

(define step-id/c symbol?)
(define handler/c (-> xexpr?))
(define transition/c (-> (or/c done? next? step-id/c)))

(define/contract (make-step id handler [transition (lambda () next)])
  (->* (step-id/c handler/c) (transition/c) step?)
  (step id handler transition))

(define/contract (make-step/study id s [transition (lambda () next)])
  (->* (step-id/c study?) (transition/c) step?)
  (step/study
   id
   (lambda ()
     (with-widget-parameterization
       (run-study s)
       (continue)))
   transition
   s))

(define/contract (wrap-sub-study s wrapper)
  (-> study? (-> handler/c handler/c) study?)
  (struct-copy study s [steps (for/list ([a-step (in-list (study-steps s))])
                                (cond
                                  [(step/study? a-step)
                                   (make-step/study
                                    (step-id a-step)
                                    (wrap-sub-study (step/study-study a-step) wrapper)
                                    (step-transition a-step))]

                                  [else
                                   (make-step
                                    (step-id a-step)
                                    (wrapper (step-handler a-step))
                                    (step-transition a-step))]))]))


;; study ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-logger study)

(define current-study-stack
  (make-parameter null))

(define current-step
  (make-parameter 'no-step))

(define/contract (make-study steps
                             #:requires [requires null]
                             #:provides [provides null])
  (->* ((non-empty-listof step?))
       (#:requires (listof symbol?)
        #:provides (listof symbol?))
       study?)
  (study requires provides steps))

(define/contract (run-study s [req (current-request)])
  (->* (study?) (request?) any)
  (parameterize ([current-study-stack (cons s (current-study-stack))])
    (begin0 (run-step req s (study-next-step s))
      (redirect/get/forget/protect))))

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
                         [current-step the-step])
            (response/xexpr ((step-handler the-step)))))))
     servlet-prompt))

  (log-study-debug "step ~.s returned ~.s" the-step res)
  (match res
    [(? response?)
     (send/back res)]

    [(cons 'to-step to-step-id)
     (define new-req (redirect/get/forget/protect))
     (define next-step (study-find-step s to-step-id))
     (unless next-step
       (error 'run-step "skipped to a nonexistent step: ~s~n  current step: ~.s~n  current study: ~.s" to-step-id the-step s))
     (run-step new-req s next-step)]

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
        (for/hasheq ([id (in-list (study-provides s))])
          (values id (get id (lambda ()
                               (error 'run-study "study did not 'put' provided variable: ~s" id)))))])]))

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
