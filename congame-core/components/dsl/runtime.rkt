#lang racket/base

(require racket/format
         racket/list
         racket/match
         "../study.rkt")

(provide
 make-put-all-keywords
 interpret-basic-expr)

(define (make-put-all-keywords [action void])
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kw (in-list kws)]
           [arg (in-list kw-args)])
       (put (string->symbol (keyword->string kw)) arg))
     (action))))

;; TODO: actual environments
(define (interpret-basic-expr e)
  (let loop ([e e])
    (match e
      [`(quote ,e) e]

      [`(lambda ,arg-ids . ,bodies)
       (lambda args
         (define ns (make-empty-namespace))
         (unless (equal? (length arg-ids) (length args))
           (error 'loop "bad arity"))
         (for ([arg-id (in-list arg-ids)]
               [arg (in-list args)])
           (namespace-set-variable-value! arg-id arg #t ns))
         (parameterize ([current-namespace ns])
           (loop `(begin . ,bodies))))]

      [`(begin . ,bodies)
       (for/last ([body-e (in-list bodies)])
         (loop body-e))]

      [`(if ,test-e ,then-e ,else-e)
       (if (loop test-e)
           (loop then-e)
           (loop else-e))]

      [`(cond [,test-e . ,bodies] [else . ,else-bodies])
       (loop `(if ,test-e
                  (begin . ,bodies)
                  (begin . ,else-bodies)))]

      [`(cond ,branch ,branches ... ,else-branch)
       (loop `(cond
                ,branch
                [else (cond
                        ,@branches
                        [else ,else-branch])]))]

      [(? boolean?) e]
      [(? number?) e]
      [(? string?) e]
      [(? symbol? id) (namespace-variable-value id)]  ;; TODO: restrict this

      [`(~a ,e) (~a (loop e))]
      [`(sub1 ,e) (sub1 (loop e))]

      [`(= ,a ,b) (= (loop a) (loop b))]
      [`(+ ,a ,b) (+ (loop a) (loop b))]
      [`(- ,a ,b) (- (loop a) (loop b))]
      [`(* ,a ,b) (* (loop a) (loop b))]
      [`(/ ,a ,b) (/ (loop a) (loop b))]

      [`(get ,id)
       (get (loop id))]

      [`(put ,id ,e)
       (put (loop id) (loop e))]

      [`(,rator . ,rands)
       (apply (loop rator) (map loop rands))]

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
