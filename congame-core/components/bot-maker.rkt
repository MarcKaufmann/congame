#lang racket/base

(require racket/contract/base
         racket/match
         "bot.rkt"
         (submod "study.rkt" private))

(provide
 model/c
 (contract-out
  [study->bot (-> study? (-> model/c bot?))]))

(define model/c
  (-> (listof symbol?) procedure? any))

(define ((study->bot s [stack '(*root*)]) model)
  (apply
   make-bot
   (for/list ([st (in-list (study-steps s))])
     (match st
       [(step/study id _ _ _ _ s)
        (make-bot-stepper/study id ((study->bot s (cons id stack)) model))]

       [(step id _ handler/bot _ _)
        (make-bot-stepper id (lambda ()
                               (parameterize ([current-study-stack stack])
                                 (define id* (reverse (cons id stack)))
                                 (with-handlers ([exn:fail?
                                                  (lambda (e)
                                                    (raise (struct-copy exn e [message (format "failure for id ~s~n~a" id* (exn-message e))])))])
                                   (model id* handler/bot)))))]))))
