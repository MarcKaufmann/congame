#lang conscript/with-require

(require koyo/haml
         racket/contract/base
         racket/lazy-require
         racket/match)

(lazy-require
 [congame-web/components/study-bot (spawn-bot)])

(provide
 (contract-out
  [make-admin-study
   (->* [study?]
        [#:models (listof (cons/c symbol? any/c))
         ; FIXME: For some reason `step?` throws an error, even after adding to conscript/base.rkt.
         #:admin any/c]
        study?)]))

(defstep (check-admin)
  (if (current-participant-owner?)
      (skip 'admin)
      (skip)))

(defstep (admin)
  @md{# Admin

      @button[#:to-step-id 'bots]{Manage Bots}
      @button[#:to-step-id 'substudy]{Participate in Study}})

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
         #:admin [admin admin]
         s)
  (define make-bot-box (box #f))
  (defstudy admin-study
    [check-admin --> [substudy s] --> ,(λ () done)]
    [admin --> admin]
    [[bots (make-bot-step make-bot-box models)] --> admin])
  (set-box! make-bot-box (bot:study->bot admin-study))
  admin-study)
