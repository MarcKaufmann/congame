#lang racket/base

(require db
         db/util/postgresql
         deta
         gregor
         json
         koyo/database
         racket/contract
         racket/sequence
         threading)

(provide
 (schema-out study-instance-data)
 list-all-study-instance-data/admin
 put-study-instance-data)

(define-schema study-instance-data
  #:table "study_instance_data"
  ([user-id id/f]
   [instance-id id/f]
   [study-stack (array/f symbol/f)]
   [key symbol/f]
   [value jsonb/f]
   [congame-url string/f]
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
                                          #:value value
                                          #:congame-url congame-url)
  (->* (database?
        #:user-id id/c
        #:instance-id id/c
        #:study-stack (listof symbol?)
        #:key symbol?
        #:value jsexpr?
        #:congame-url string?)
       ()
       void?)
  ; FIXME: The constraint needs to be updated to check for the congame server
  (with-database-connection [conn db]
    (query-exec conn #<<SQL
INSERT INTO study_instance_data (
  user_id, instance_id, congame_url, study_stack, key, value, last_put_at
) VALUES (
  $1, $2, $3, $4, $5, $6, CURRENT_TIMESTAMP
) ON CONFLICT (
  user_id, instance_id, study_stack, key
) DO UPDATE SET
  value = EXCLUDED.value,
  last_put_at = CURRENT_TIMESTAMP
SQL
                user-id
                instance-id
                congame-url
                (list->pg-array (map symbol->string study-stack))
                (symbol->string key)
                value)))

(define/contract (list-all-study-instance-data/admin db)
  (-> database? (listof study-instance-data?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from study-instance-data #:as i)
                           (order-by ([i.instance-id #:desc]
                                      [i.user-id]
                                      [i.key #:asc])))))))
