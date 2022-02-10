#lang racket/base

(require db
         db/util/postgresql
         deta
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
        #:study-stack (listof symbol?)
        #:key symbol?
        #:value jsexpr?)
       ()
       void?)
  (with-database-connection [conn db]
    (query-exec conn #<<SQL
INSERT INTO study_instance_data (
  user_id, instance_id, study_stack, key, value, last_put_at
) VALUES (
  $1, $2, $3, $4, $5, CURRENT_TIMESTAMP
) ON CONFLICT (
  user_id, instance_id, study_stack, key
) DO UPDATE SET
  value = EXCLUDED.value,
  last_put_at = CURRENT_TIMESTAMP
SQL
                user-id
                instance-id
                (list->pg-array (map symbol->string study-stack))
                (symbol->string key)
                value)))
