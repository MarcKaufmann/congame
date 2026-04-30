#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre)
         component
         congame/components/dsl
         congame/components/study
         db
         deta
         koyo/database
         koyo/job
         racket/lazy-require
         threading)

(lazy-require
 [congame-web/components/user (user-admin-like?)]
 [congame-web/components/upload (call-with-uploader)])

(provide
 (rename-out
  [define-conscript-job define-job]))

(define-syntax (define-conscript-job stx)
  (syntax-parse stx
    [(_ (id:id arg ...) body:expr ...+)
     #:with impl-id (format-id #'id "~a-impl" #'id)
     #'(begin
         (provide impl-id)
         (define (impl-id arg ...)
           body ...)
         (define id
           (make-keyword-procedure
            (lambda (kws kw-args . args)
              (conscript-dyn-job
               #:participant-id (current-participant-id)
               #:procedure-id 'impl-id
               kws kw-args args)))))]))

(define-job (conscript-dyn-job
             #:participant-id participant-id
             #:procedure-id proc-id
             kws kw-args args)
  (define db (system-ref 'db))
  (define uploader
    (system-ref 'uploader))
  (define participant
    (with-database-connection [conn db]
      (~> (from study-participant #:as p)
          (where (= p.id ,participant-id))
          (lookup conn _))))
  (unless participant
    (error 'conscript-dyn-job "participant ~s not found" participant-id))
  (define meta
    (with-database-connection [conn db]
      (~> (from study-meta #:as m)
          (join study-instance #:as i #:on (= m.id i.study-id))
          (where (= i.id ,(study-participant-instance-id participant)))
          (lookup conn _))))
  (unless meta
    (error 'conscript-dyn-job "study for participant ~s not found" participant-id))
  (unless (eq? (study-meta-type meta) 'dsl)
    (error 'conscript-dyn-job "study for participant ~s is not an uploaded study" participant-id))
  (define owner
    (with-database-connection [conn db]
      (~> (from user #:as u)
          (where (= u.id ,(study-meta-owner-id meta)))
          (lookup conn _))))
  (unless owner
    (error 'conscript-dyn-job "study for participant ~s has no owner" participant-id))
  (define proc
    (call-with-uploader
     uploader
     (lambda ()
       (dsl-require
        (if (sql-null? (study-meta-dsl-archive-path meta))
            (study-meta-dsl-source meta)
            `(archive ,(study-meta-dsl-archive-path meta)))
        #;id proc-id
        #;owner-is-admin? (user-admin-like? owner)))))
  (call-with-study-manager
   (make-study-manager
    #:database db
    #:participant participant)
   (lambda ()
     (keyword-apply proc kws kw-args args))))
