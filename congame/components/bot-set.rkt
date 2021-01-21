#lang racket/base

(require deta
         gregor
         koyo/database)

(provide
 (schema-out bot-set)
 create-bot-set!)

(define-schema bot-set
  #:table "bot_sets"
  ([id integer/f #:primary-key #:auto-increment]
   [study-id integer/f]
   [study-instance-id integer/f]
   [bot-id symbol/f]
   [model-id symbol/f]
   [bot-count integer/f]
   [(created-at (now/moment)) datetime-tz/f]))

(define create-bot-set!
  (make-keyword-procedure
   (lambda (kws kw-args db)
     (with-database-connection [conn db]
       (insert-one! conn (keyword-apply make-bot-set kws kw-args null))))))
