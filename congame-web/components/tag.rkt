#lang racket/base

(require db
         deta
         koyo/database
         racket/contract
         racket/sequence
         racket/string
         threading)

(provide
 (schema-out tag)
 list-tags
 lookup-tag
 create-tag!
 get-tag-instance-ids
 associate-tags!)

(define-schema tag
  #:table "tags"
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]))

(define-schema study-instance-tag
  #:table "study_instance_tags"
  ([study-instance-id id/f]
   [tag-id id/f]))

(define/contract (list-tags db)
  (-> database? (listof tag?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from tag #:as t)
                           (order-by ([name #:asc])))))))

(define/contract (lookup-tag db id)
  (-> database? id/c (or/c #f tag?))
  (with-database-connection [conn db]
    (lookup conn (~> (from tag #:as t)
                     (where (= t.id ,id))))))

(define/contract (create-tag! db name)
  (-> database? non-empty-string? tag?)
  (with-database-connection [conn db]
    (insert-one! conn (make-tag #:name name))))

(define/contract (get-tag-instance-ids db tag-id)
  (-> database? id/c (listof id/c))
  (with-database-connection [conn db]
    (for/list ([(id) (in-query conn (~> (from study-instance-tag #:as a)
                                        (where (= a.tag-id ,tag-id))
                                        (select a.study-instance-id)))])
      id)))

(define/contract (associate-tags! db tag-id instance-ids)
  (-> database? id/c (listof id/c) void?)
  (with-database-transaction [conn db]
    (query-exec conn (~> (from study-instance-tag #:as a)
                         (where (= a.tag-id ,tag-id))
                         (delete)))
    (apply insert! conn (for/list ([id (in-list instance-ids)])
                          (make-study-instance-tag
                           #:tag-id tag-id
                           #:study-instance-id id)))
    (void)))
