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
  (let loop ([s s] [stack stack])
    (cond
      [(procedure? s)
       (make-bot
        (make-bot-stepper/delay
         (car stack)
         (lambda ()
           ;; Delay the bot creation until runtime. Will behave in
           ;; unexpected ways for any study that doesn't generate
           ;; substudies deterministially per participant.
           (loop (s) stack))))]
      [else
       (apply
        make-bot
        (for/list ([st (in-list (study-steps s))])
          (match st
            [(step/study id _ _ _ _ s)
             (if (procedure? s)
                 (make-bot-stepper/delay
                  #;id id
                  #;bot (lambda ()
                          (loop (s) stack)))
                 (make-bot-stepper/study
                  #;id id
                  #;bot (loop s (cons id stack))))]

            [(step id _ handler/bot _ _)
             (make-bot-stepper
              #;id id
              #;action
              (lambda ()
                (parameterize ([current-study-stack stack])
                  (define id* (reverse (cons id stack)))
                  (with-handlers ([exn:fail?
                                   (lambda (e)
                                     (raise (struct-copy exn e [message (format "failure for id ~s~n~a" id* (exn-message e))])))])
                    (model id* handler/bot)))))])))])))
