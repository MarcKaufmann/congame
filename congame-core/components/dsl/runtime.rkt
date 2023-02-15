#lang racket/base

(require racket/format
         racket/list
         racket/match
         "../study.rkt")

(provide
 make-put-all-keywords)

(define (make-put-all-keywords [action void])
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kw (in-list kws)]
           [arg (in-list kw-args)])
       (put (string->symbol (keyword->string kw)) arg))
     (action))))


;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-environment
 make-initial-environment
 environment?
 environment-set!
 environment-ref)

(struct environment (parent bindings))

(define-syntax-rule (define-initial-env env-id id ...)
  (define env-id
    (make-hasheq
     (list (cons 'id id) ...))))

(define-initial-env initial-env
  + - * / =
  zero? even?
  get put
  get/instance put/instance
  hash hash-ref hash-set hash-update
  add1 sub1
  ~a format number->string string->number
  random
  done next)

(define (make-initial-environment)
  (define env (make-environment))
  (begin0 env
    (for ([(id v) (in-hash initial-env)])
      (environment-set! env id v))))

(define (make-environment [parent #f])
  (environment parent (make-hasheq)))

(define (environment-set! e id v)
  (hash-set! (environment-bindings e) id v))

(define (environment-ref e id)
  (hash-ref (environment-bindings e) id (λ ()
                                          (cond
                                            [(environment-parent e) => (λ (pe) (environment-ref pe id))]
                                            [else (error 'environment-ref "unbound variable ~a" id)]))))

(define (environment-has-binding? e id)
  (hash-has-key? (environment-bindings e) id))


;; interpreter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 interpret)

(define (interpret e [env (make-initial-environment)])
  (let loop ([e e] [env env])
    (match e
      [`(quote ,e) e]

      [`(lambda ,arg-ids . ,bodies)
       (lambda args
         (unless (equal? (length arg-ids) (length args))
           (error 'interpret "bad arity"))
         (define lambda-env (make-environment env))
         (for ([arg-id (in-list arg-ids)]
               [arg (in-list args)])
           (environment-set! lambda-env arg-id arg))
         (loop `(begin . ,bodies) lambda-env))]

      [`(begin . ,bodies)
       (for/last ([body-e (in-list bodies)])
         (loop body-e env))]

      [`(define ,id ,e)
       (unless (symbol? id)
         (error 'interpret "id must be a symbol"))
       (when (environment-has-binding? env id)
         (error 'interpret "cannot redefine variable ~a" id))
       (environment-set! env id (loop e env))]

      [`(if ,test-e ,then-e ,else-e)
       (if (loop test-e env)
           (loop then-e env)
           (loop else-e env))]

      [`(cond [,test-e . ,bodies] [else . ,else-bodies])
       (loop `(if ,test-e
                  (begin . ,bodies)
                  (begin . ,else-bodies))
             env)]

      [`(cond ,branch ,branches ... ,else-branch)
       (loop `(cond
                ,branch
                [else (cond
                        ,@branches
                        [else ,else-branch])])
             env)]

      [(? boolean?) e]
      [(? number?) e]
      [(? string?) e]
      [(? symbol? id) (environment-ref env id)]

      [`(,rator . ,rands)
       (apply (loop rator env) (map (λ (rand) (loop rand env)) rands))]

      [_ (error 'interpret-basic-expr "invalid expression: ~e" e)])))


;; randomization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-randomized-study)

;; NOTE: Someone could provide a step named
;; `synthetic-randomizer-step`, in which case it will error.
(define (make-randomized-study steps)
  (define (randomize)
    (define shuffled-steps (shuffle (hash-keys steps)))
    (put 'steps shuffled-steps)
    (put 'remaining-steps shuffled-steps)
    (skip))
  (make-study
   "randomized-study"
   (cons
    (make-step 'synthetic-randomizer-step randomize randomizer-transition)
    (for/list ([(id step) (in-hash steps)])
      (make-step id step randomizer-transition)))))

(define (randomizer-transition)
  (let ([steps (get 'remaining-steps)])
    (cond
      [(null? steps)
       done]
      [else
       (put 'remaining-steps (cdr steps))
       (car steps)])))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ->step)

(define (->step id v)
  (cond
    [(study? v) (make-step/study id v)]
    [(or (step/study? v) (step? v)) v]
    [else (make-step id v)]))
