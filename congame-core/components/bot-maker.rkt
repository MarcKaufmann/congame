#lang racket/base

(require racket/contract
         racket/match
         "bot.rkt"
         (submod "study.rkt" private))

(provide
 study->bot)

(define/contract ((study->bot s [stack '(*root*)]) get-input)
  (-> study? (-> (-> (listof symbol?) any) bot?))
  (apply
   make-bot
   (for/list ([st (in-list (study-steps s))])
     (match st
       [(step/study id _ _ _ s)
        (make-bot-stepper/study id ((study->bot s (cons id stack)) get-input))]

       [(step id _ handler/bot _)
        (make-bot-stepper id (lambda ()
                               (define id* (reverse (cons id stack)))
                               (with-handlers ([exn:fail:contract:arity?
                                                (lambda (_e)
                                                  (raise-user-error (format "step ~s expects ~s arguments" id* (procedure-arity handler/bot))))]
                                               [exn:fail?
                                                (lambda (e)
                                                  (raise (struct-copy exn e [message (format "failure for id ~s~n~a" id* (exn-message e))])))])
                                 (call-with-values
                                  (lambda ()
                                    (get-input id*))
                                  handler/bot))))]))))
