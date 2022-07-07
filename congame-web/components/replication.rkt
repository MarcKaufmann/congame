#lang racket/base

(require component
         congame-web/components/user
         congame/components/study
         db
         deta
         gregor
         koyo/database
         koyo/database/migrator
         koyo/hasher
         koyo/random
         (prefix-in http: net/http-easy)
         racket/contract
         racket/format
         racket/sequence
         threading)

(provide
 replication-manager?
 make-replication-manager
 (schema-out replication)
 create-replication!)

(struct replication-manager (db hasher migrations-path)
  #:transparent)

(define/contract (make-replication-manager db hasher migrations-path)
  (-> database? hasher? path? replication-manager?)
  (replication-manager db hasher migrations-path))

(define-schema replication
  #:table "replications"
  ([id id/f #:primary-key #:auto-increment]
   [slug string/f]
   [git-sha string/f]
   [instance-ids (array/f id/f)]
   [docker-container-port integer/f #:contract (integer-in 0 65535)]
   [docker-container-id string/f]
   [(created-at (now/moment)) datetime-tz/f]))

(define/contract (create-replication! manager
                                      #:slug slug
                                      #:git-sha git-sha
                                      #:admin-username admin-username
                                      #:admin-password admin-password
                                      #:instance-ids instance-ids)
  (-> replication-manager?
      #:slug string?
      #:git-sha string?
      #:admin-username string?
      #:admin-password string?
      #:instance-ids (listof id/c)
      replication?)
  (with-database-transaction [conn (replication-manager-db manager)]
    (define rep
      (insert-one! conn
                   (make-replication
                    #:slug slug
                    #:git-sha git-sha
                    #:instance-ids (list->vector instance-ids)
                    #:docker-container-port 0
                    #:docker-container-id "")))
    (define db-name (~a "congame_replication_" (replication-id rep)))
    (define db-password (generate-random-string))
    (create&replicate-db!
     manager
     #:database-name db-name
     #:database-password db-password
     #:admin-username admin-username
     #:admin-password admin-password
     #:instance-ids instance-ids)
    rep
    #;
    (let ()
      (define container-port (+ 9500 (replication-id rep))) ;; FIXME: needs to be unique across deployment types
      (define container-name (~a "congame_replication_" (replication-id rep)))
      (define container-image (~a "ghcr.io/marckaufmann/congame-web:" git-sha))
      (define container-env null) ;; FIXME
      (define container-ports (hash container-port container-port))
      (define container-volumes (hash))
      (define container-id
        (launch-container!
         container-name container-image
         #:env container-env
         #:ports container-ports
         #:volumes container-volumes))
      (update-one! conn (~> rep
                            (set-replication-docker-container-port _ container-port)
                            (set-replication-docker-container-id _ container-id))))))

(define (create&replicate-db! manager
                              #:database-name name
                              #:database-password password
                              #:admin-username admin-username
                              #:admin-password admin-password
                              #:instance-ids instance-ids)
  (parameterize ([current-database-connection #f])  ;; intentionally "escape" the outer transaction
    (with-database-connection [src-conn (replication-manager-db manager)]
      (query-exec src-conn (format "CREATE USER ~a PASSWORD '~a'" name password))
      (query-exec src-conn (format "CREATE DATABASE ~a" name))
      (query-exec src-conn (format "GRANT ALL PRIVILEGES ON DATABASE ~a TO ~a" name name))
      (define dst-db
        (component-start
         ((make-database-factory
           (λ ()
             (postgresql-connect
              #:database name
              #:user name
              #:password password))))))
      (parameterize ([current-database-connection #f])  ;; ditto
        (component-start ((make-migrator-factory (replication-manager-migrations-path manager)) dst-db))
        (with-database-transaction [dst-conn dst-db]
          (define users (make-user-manager dst-db (replication-manager-hasher manager)))
          (define admin-user (user-manager-create! users admin-username admin-password #(admin)))
          (define (copy-rows! table-name id-accessor id-setter query [proc values])
            (define seq-name (format "~a_id_seq" table-name))
            (define src-data
              (sequence->list
               (in-entities src-conn query)))
            (define dst-data
              (apply insert! dst-conn (for/list ([e (in-list src-data)])
                                        ;; Force the dirty flag to be set on the
                                        ;; entity so that deta will insert it.
                                        (proc (id-setter e (id-accessor e))))))
            (for ([src-e (in-list src-data)]
                  [dst-e (in-list dst-data)])
              (query-exec dst-conn
                          (format "UPDATE ~a SET id = $1 WHERE id = $2" table-name)
                          (+ (id-accessor src-e) (* 1 1000 1000))
                          (id-accessor dst-e)))
            (query-exec dst-conn
                        (format "UPDATE ~a SET id = id - $1 WHERE id = ANY($2)" table-name)
                        (* 1 1000 1000)
                        (map (λ (e) (+ (id-accessor e) (* 1 1000 1000))) src-data))
            (query-exec dst-conn
                        (format "SELECT setval('~a', $1)" seq-name)
                        (query-value src-conn (format "SELECT last_value FROM ~a" seq-name))))
          (println name)
          ;; copy studies
          (copy-rows!
           "studies"
           study-meta-id
           set-study-meta-id
           (~> (from study-meta #:as s)
               (where (in s.id (subquery
                                (~> (from "study_instances" #:as si)
                                    (select si.study-id)
                                    (where (in si.id ,@instance-ids))))))))

          ;; copy study-instances
          (copy-rows!
           "study_instances"
           study-instance-id
           set-study-instance-id
           (~> (from study-instance #:as si)
               (where (in si.id ,@instance-ids)))
           (lambda (i)
             (set-study-instance-owner-id i (user-id admin-user))))

          ;; copy participant users
          (copy-rows!
           "users"
           user-id
           set-user-id
           (~> (from user #:as u)
               (where (in u.id (subquery
                                (~> (from "study_participants" #:as p)
                                    (select p.user-id)
                                    (where (in p.instance-id ,@instance-ids)))))))
           (lambda (u)
             (~> (set-user-roles u #(user))
                 (set-user-username _ (generate-random-string))
                 (set-user-password-hash _ "")
                 (set-user-verification-code _ (generate-random-string))
                 (set-user-bot-set-id _ sql-null)
                 (set-user-api-key _ ""))))

          ;; copy participants
          #;
          (copy-rows!
           "study_participants"
           study-participant-id
           set-study-participant-id
           (~> (from study-participant #:as p)
               (where (in p.instance-id ,@instance-ids)))))))))

(define (launch-container! name image
                           #:env [env null]
                           #:ports [ports (hash)]
                           #:volumes [volumes (hash)]
                           #:network [network 'congame])
  (define res
    (http:post
     (docker-uri "/containers/create")
     #:params `((name . ,name))
     #:json (hasheq
             'Image image
             'Env env
             'Volumes volumes
             'HostConfig (hasheq
                          'PortBindings (for/hasheq ([(host-port container-port) (in-hash ports)])
                                          (values
                                           (string->symbol (~a container-port))
                                           (hasheq 'HostPort (~a host-port)))))
             'NetworkingConfig (hasheq 'EndpointsConfig (hasheq network (hasheq))))))
  (unless (= (http:response-status-code res) 201)
    (error 'launch-container!
           "failed to launch container~n status code: ~a~n message: ~a"
           (http:response-status-code res)
           (hash-ref (http:response-json res) 'message)))
  (define container-id
    (hash-ref (http:response-json res) 'Id))
  (begin0 container-id
    (start-container! container-id)))

(define (start-container! id)
  (define res
    (http:post
     (docker-uri (format "/containers/~a/start" id))))
  (unless (= (http:response-status-code res) 204)
    (error 'start-container!
           "failed to start container~n status code: ~a~n message: ~a"
           (http:response-status-code res)
           (hash-ref (http:response-json res) 'message))))

(define (docker-uri path)
  (~a "http+unix://%2Fvar%2Frun%2Fdocker.sock/v1.41" path))
