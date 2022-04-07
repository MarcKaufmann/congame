#lang racket/base

(require deta
         gregor
         koyo/database
         koyo/url
         (prefix-in http: net/http-easy)
         racket/contract
         racket/format
         racket/sequence
         racket/string
         threading
         (prefix-in config: "../config.rkt")
         "shadow.rkt"
         "user.rkt")

(provide
 (schema-out congame-server)
 (schema-out study-instance)
 congame-server-enroll-user!
 congame-server-study-instances
 congame-server-tags
 get-congame-server
 get-congame-servers)

(define-schema congame-server
  #:table "congame_servers"
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [url string/f #:contract non-empty-string?]
   [key string/f #:contract non-empty-string?]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])

  #:pre-persist-hook
  (lambda (u)
    (set-congame-server-updated-at u (now/moment))))

(define-schema study-instance
  #:virtual
  ([id id/f]
   [name string/f]
   [status symbol/f #:contract (or/c 'active 'inactive 'archived)]))

(define/contract (congame-server-auth cs)
  (-> congame-server? http:auth-procedure/c)
  (λ (_url headers params)
    (values (hash-set headers 'authorization (congame-server-key cs)) params)))

(define/contract (congame-server-path cs . args)
  (-> congame-server? string? ... string?)
  (apply ~a (congame-server-url cs) args))

(define (response-json/success res api-name)
  (define data (http:response-json res))
  (cond [(= (http:response-status-code res) 200)
         data]
        [else
         (error api-name "API error: ~a" (hash-ref data 'error))]))

(define/contract (congame-server-study-instances cs)
  (-> congame-server? (listof study-instance?))
  (define res
    (http:get (congame-server-path cs "/api/v1/studies.json")
              #:auth (congame-server-auth cs)))
  (define data
    (response-json/success res 'congame-server-study-instances))
  (for*/list ([study-data (in-list (hash-ref data 'studies))]
              [instance-data (in-list (hash-ref study-data 'instances))]
              #:when (string=? (hash-ref instance-data 'status) "active"))
    (make-study-instance
     #:id (hash-ref instance-data 'id)
     #:name (hash-ref instance-data 'name)
     #:status (string->symbol (hash-ref instance-data 'status)))))

(define/contract (congame-server-enroll-user! db cs u instance-id)
  (-> database? congame-server? user? id/c string?)
  (define the-shadow
    (with-database-transaction [conn db]
      (define maybe-shadow
        (lookup conn (~> (from shadow #:as s)
                         (where (and (= s.user-id ,(user-id u))
                                     (= s.server-id ,(congame-server-id cs))
                                     (= s.instance-id ,instance-id))))))
      (or maybe-shadow
          (insert-one! conn (make-shadow
                             #:user-id (user-id u)
                             #:server-id (congame-server-id cs)
                             #:instance-id instance-id)))))

  (define data
    (response-json/success
     (http:post (congame-server-path cs "/api/v1/study-participants-with-identity")
                #:auth (congame-server-auth cs)
                #:json (hasheq
                        'instance-id instance-id
                        'identity-email (format "~a@~a" (shadow-display-name the-shadow) config:domain-name)
                        'identity-domain (make-application-url)
                        'identity-key (shadow-api-key the-shadow)))
     'congame-server-enroll-user!))
  (congame-server-path cs (hash-ref data 'target-path)))

(define/contract (congame-server-tags cs)
  (-> congame-server? (hash/c any/c any/c))
  (define data
    (response-json/success
     (http:get (congame-server-path cs "/api/v1/tags.json")
               #:auth (congame-server-auth cs))
     'congame-server-tags))
  ;; json does not allow numbers as hash keys, so that the data we receive has symbolic keys in the form of '|2| for the integer key 2. This converts them back to numbers.
  (for/hash ([(key value) (in-hash (hash-ref data 'tags))])
    (values (string->number
             (symbol->string key))
            value)))

(define/contract (get-congame-server db id)
  (-> database? id/c (or/c #f congame-server?))
  (with-database-connection [conn db]
    (lookup conn (~> (from congame-server #:as cs)
                     (where (= cs.id ,id))))))

(define/contract (get-congame-servers db)
  (-> database? (listof congame-server?))
  (with-database-connection [conn db]
    (sequence->list
     (in-entities conn (~> (from congame-server #:as cs)
                           (order-by ([cs.updated-at #:desc])))))))
