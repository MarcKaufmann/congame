#lang conscript

(require congame-web/components/study-bot
         koyo/haml
         racket/contract/base
         racket/match)

(provide
 (contract-out
  [make-admin-study
   (->* [study?]
        [#:models (listof (cons/c symbol? any/c))]
        study?)]))

(defstep (admin)
  (if (current-participant-owner?)
      @md{# Admin

          @button[#:to-step-id 'bots]{Manage Bots}
          @button{Participate in Study}}
      (skip)))

;; '(intro substudy *root*) -> '(intro *root*)
(define (rebase k)
  (reverse (cons '*root* (drop (reverse k) 2))))

(define ((nest-model m) k proc)
  (m (rebase k) proc))

(define ((make-bot-step bot-box models))
  (define ((make-spawn-cb model))
    (spawn-bot
     ((unbox bot-box)
      (nest-model model))))

  @md{# Bots

      @(haml
        (:table
         (:thead
          (:tr
           (:th "Model")
           (:th "Actions")))
         (:tbody
          ,@(for/list ([model (in-list models)])
              (match-define (cons id m) model)
              (define label (symbol->string id))
              (haml
               (:tr
                (:td label)
                (:td (button
                      #:to-step-id 'bots
                      (make-spawn-cb m)
                      "Spawn"))))))))

      @button[#:to-step-id 'admin]{Back to admin...}})

(define (make-admin-study
         #:models [models null]
         s)
  (define make-bot-box (box #f))
  (defstudy admin-study
    [admin --> [substudy s] --> ,(λ () done)]
    [[bots (make-bot-step make-bot-box models)] --> admin])
  (set-box! make-bot-box (bot:study->bot admin-study))
  admin-study)
