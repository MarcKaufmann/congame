#lang racket/base

(require db
         deta
         koyo/database
         racket/contract
         racket/sequence
         threading)

(provide
 (schema-out instance-link/admin)
 (contract-out
  [list-instance-links/admin
   (-> database? id/c (listof instance-link/admin?))]
  [delete-instance-link/admin
   (-> database? instance-link/admin? void?)]))

(define-schema instance-link/admin
  #:virtual
  ([this-id id/f]
   [other-id id/f]
   [other-study-id id/f]
   [other-name string/f]
   [pseudonym string/f]
   [relationship symbol/f]
   [linked? boolean/f]))

(define (list-instance-links/admin db instance-id)
  (define query
    (~> (from study-instance-link #:as this-link)
        (join study-instance #:as other #:on (= other.id this-link.study-instance-id-b))
        (where (= this-link.study-instance-id-a ,instance-id))
        (select
         this-link.study-instance-id-a
         other.id
         other.study-id
         other.name
         this-link.pseudonym-b
         this-link.relationship
         (subquery
          (~> (from study-instance-link #:as other-link)
              (where (and (= other-link.study-instance-id-b this-link.study-instance-id-a)
                          (cond
                            [(= this-link.relationship "source")
                             (= other-link.relationship "reporter")]
                            [(= this-link.relationship "reporter")
                             (= other-link.relationship "source")]
                            [else #f])))
              (select (> (count *) 0)))))
        (project-onto instance-link/admin-schema)))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn query))))

(define (delete-instance-link/admin db l)
  (with-database-connection [conn db]
    (query-exec conn (~> (from study-instance-link #:as l)
                         (where (and (= l.study-instance-id-a ,(instance-link/admin-this-id l))
                                     (= l.study-instance-id-b ,(instance-link/admin-other-id l))
                                     (= l.pseudonym-b ,(instance-link/admin-pseudonym l))))
                         (delete)))))
