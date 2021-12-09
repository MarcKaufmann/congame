#lang racket/base

(require racket/contract
         racket/match
         "bot.rkt"
         (submod "study.rkt" private))

(provide
 study->bot)

(define model/c
  (-> (listof symbol?) procedure? any))

(define/contract ((study->bot s [stack '(*root*)]) model)
  (-> study? (-> model/c bot?))
  (apply
   make-bot
   (for/list ([st (in-list (study-steps s))])
     (match st
       [(step/study id _ _ _ s)
        (make-bot-stepper/study id ((study->bot s (cons id stack)) model))]

       [(step id _ handler/bot _)
        (make-bot-stepper id (lambda ()
                               (define id* (reverse (cons id stack)))
                               (with-handlers ([exn:fail?
                                                (lambda (e)
                                                  (raise (struct-copy exn e [message (format "failure for id ~s~n~a" id* (exn-message e))])))])
                                 (model id* handler/bot))))]))))
