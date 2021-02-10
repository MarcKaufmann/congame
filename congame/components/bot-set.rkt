#lang racket/base

(require congame/components/study
         db
         deta
         gregor
         koyo/database
         koyo/random
         racket/contract
         threading
         congame-web/components/user) ;"user.rkt")

(provide
 (schema-out bot-set)
 create-bot-set!
 lookup-bot-set
 list-bot-sets
 prepare-bot-set!)

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
     (with-database-transaction [conn db]
       (define the-set
         (insert-one! conn (keyword-apply make-bot-set kws kw-args null)))
       (define the-users
         (for/list ([id (in-range (bot-set-bot-count the-set))])
           (make-user
            #:username (format "bot-~a-~a-~a-~a@example.com"
                               (bot-set-study-id the-set)
                               (bot-set-study-instance-id the-set)
                               (bot-set-id the-set)
                               id)
            #:role 'bot
            #:verified? #t
            #:bot-set-id (bot-set-id the-set))))

       (for ([u (in-list (apply insert! conn the-users))])
         (enroll-participant! db (user-id u) (bot-set-study-instance-id the-set)))
       the-set))))

(define/contract (lookup-bot-set db id)
  (-> database? id/c (or/c #f bot-set?))
  (with-database-connection [conn db]
    (lookup conn (~> (from bot-set #:as s)
                     (where (= s.id ,id))))))

(define/contract (list-bot-sets db study-instance-id)
  (-> database? id/c (listof bot-set?))
  (with-database-connection [conn db]
    (for/list ([bs (in-entities conn (~> (from bot-set #:as s)
                                         (where (= s.study-instance-id ,study-instance-id))
                                         (order-by ([s.created-at]))))])
      bs)))

(define/contract (prepare-bot-set! db the-set)
  (-> database? bot-set? (values string? (listof user?)))
  (with-database-transaction [conn db]
    (define password
      (generate-random-string 64))
    (define updated-users
      (for/list ([u (in-entities conn (~> (from user #:as u)
                                          (where (= u.bot-set-id ,(bot-set-id the-set)))))])
        (define participant-id
          (query-value conn (~> (from "study_participants" #:as p)
                                (select id)
                                (where (= p.user-id ,(user-id u))))))
        (clear-participant-progress! db participant-id)
        (set-user-password u password)))
    (values password (apply update! conn updated-users))))
