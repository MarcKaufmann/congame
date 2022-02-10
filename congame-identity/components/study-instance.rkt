#lang racket/base

(require deta
         gregor
         json
         koyo/database
         racket/contract)

(provide
 (schema-out study-instance-data)
 put-study-instance-data)

(define-schema study-instance-data
  #:table "study_instance_data"
  ([user-id id/f]
   [instance-id id/f]
   [study-stack (array/f symbol/f)]
   [key symbol/f]
   [value jsonb/f]
   [(last-put-at (now/moment)) datetime-tz/f]
   [(first-put-at (now/moment)) datetime-tz/f])
  #:pre-persist-hook
  (lambda (e)
    (set-study-instance-data-last-put-at e (now/moment))))

(define/contract (put-study-instance-data db
                                          #:user-id user-id
                                          #:instance-id instance-id
                                          #:study-stack study-stack
                                          #:key key
                                          #:value value)
  (->* (database?
        #:user-id id/c
        #:instance-id id/c
        #:study-stack (vectorof symbol?)
        #:key symbol?
        #:value jsexpr?)
       ()
       void?)
  (define d
    (make-study-instance-data
     #:user-id user-id
     #:instance-id instance-id
     #:study-stack study-stack
     #:key key
     #:value value))
  (void
   (with-database-connection [conn db]
     (with-handlers ([exn:fail:sql:constraint-violation?
                      (lambda (_)
                        (update-one! conn d))])
       (insert-one! conn d)))))
