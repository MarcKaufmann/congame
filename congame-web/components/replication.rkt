#lang racket/base

(require component
         congame-web/components/user
         congame/components/study
         db
         db/util/postgresql
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
    (define environment (getenv "CONGAME_ENVIRONMENT"))
    (define staging? (equal? environment "staging"))
    (define environment-prefix (if staging? "staging-" ""))
    (define db-name (~a environment-prefix "congame_replication_" (replication-id rep)))
    (define db-password (generate-random-string))
    (create&replicate-db!
     manager
     #:database-host (or (getenv "CONGAME_DB_HOST") "127.0.0.1")
     #:database-port (string->number (or (getenv "CONGAME_DB_PORT") "5432"))
     #:database-name db-name
     #:database-password db-password
     #:admin-username admin-username
     #:admin-password admin-password
     #:instance-ids instance-ids)
    (define container-port
      (+ 9500 (+ (* (replication-id rep) 2) (if staging? 1 0))))
    (define container-name (~a "congame_replication_" (replication-id rep)))
    (define container-image (~a "ghcr.io/marckaufmann/congame-web:" git-sha))
    (define (env id)
      (and~> (getenv id)
             (format "~a=~a" id _)))
    (define container-env
      (filter
       values
       (list
        "CONGAME_LOG_LEVEL=debug"
        "CONGAME_URL_SCHEME=https"
        (format "CONGAME_URL_HOST=~areplication-~a.totalinsightmanagement.com"
                environment-prefix
                (replication-id rep))
        "CONGAME_URL_PORT=443"
        "CONGAME_HTTP_HOST=0.0.0.0"
        "CONGAME_HTTP_PORT=8000"
        (env "CONGAME_DB_HOST")
        (env "CONGAME_DB_PORT")
        (env "CONGAME_DB_NAME")
        (env "CONGAME_DB_USERNAME")
        (env "CONGAME_DB_PASSWORD")
        "CONGAME_ENVIRONMENT=replication"
        "CONGAME_PRODUCT_NAME=totalinsightmanagement.com"
        "CONGAME_SUPPORT_NAME=\"Marc Kaufmann\""
        "CONGAME_SUPPORT_EMAIL=admin@totalinsightmanagement.com"
        "CONGAME_IDENTITY_URL=identity.totalinsightmanagement.com"
        (env "CONGAME_UPLOADS_DIR")
        "PLTSTDERR=error debug@GC")))
    (define container-ports (hash container-port "8000"))
    (define container-volumes
      (filter
       values
       (list
        (let ([uploads-path (getenv "CONGAME_UPLOADS_DIR")])
          (and uploads-path (~a uploads-path ":" uploads-path))))))
    (define container-id
      (launch-container!
       container-name container-image
       #:env container-env
       #:ports container-ports
       #:volumes container-volumes
       #:links (case environment
                 [("staging" "production")
                  (list (getenv "CONGAME_DB_HOST"))]
                 [else
                  null])))
    (update-one! conn (~> rep
                          (set-replication-docker-container-port _ container-port)
                          (set-replication-docker-container-id _ container-id)))))

(define (create&replicate-db! manager
                              #:database-host database-host
                              #:database-port database-port
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
              #:server database-host
              #:port database-port
              #:database name
              #:user name
              #:password password))))))
      (parameterize ([current-database-connection #f])  ;; ditto
        ;; NOTE: Migrations might be backwards-incompatible with old shas.
        (component-start ((make-migrator-factory (replication-manager-migrations-path manager)) dst-db))
        (with-database-transaction [dst-conn dst-db]
          (define users (make-user-manager dst-db (replication-manager-hasher manager)))
          (define admin-user
            (let ([id (query-value src-conn "SELECT last_value + 1 FROM users_id_seq")]
                  [u (user-manager-create! users admin-username admin-password #(admin))])
              (query-exec dst-conn "UPDATE users SET id = $1, is_verified = TRUE WHERE id = $2" id (user-id u))
              (lookup dst-conn (~> (from user #:as u)
                                   (where (= u.id ,id))))))

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
                          (- (id-accessor src-e))
                          (id-accessor dst-e)))
            (query-exec dst-conn (format "UPDATE ~a SET id = -id WHERE id < 0" table-name))
            (query-exec dst-conn
                        (format "SELECT setval('~a', $1)" seq-name)
                        (query-value src-conn (format "SELECT last_value + 1 FROM ~a" seq-name))))

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
          (copy-rows!
           "study_participants"
           study-participant-id
           set-study-participant-id
           (~> (from study-participant #:as p)
               (where (and (in p.instance-id ,@instance-ids)
                           (not (is p.user-id null))))))

          ;; copy participant data
          (sequence-for-each
           (λ columns
             (apply query-exec
                    dst-conn
                    (~a "INSERT INTO study_data("
                        "  participant_id, study_stack, key, value, git_sha, last_put_at, first_put_at, round_name, group_name"
                        ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)")
                    columns))
           (in-query src-conn
                     (~a "SELECT"
                         "  participant_id, study_stack, key, value, git_sha, last_put_at, first_put_at, round_name, group_name"
                         " FROM"
                         "  study_data"
                         " WHERE"
                         "  participant_id = ANY($1)")
                     (list->pg-array
                      (query-list dst-conn
                                  (~> (from "study_participants" #:as p)
                                      (select p.id))))))

          ;; copy participant instance data
          (sequence-for-each
           (λ columns
             (apply query-exec
                    dst-conn
                    (~a "INSERT INTO study_instance_data("
                        "  instance_id, study_stack, key, value, git_sha, last_put_at, first_put_at"
                        ") VALUES ($1, $2, $3, $4, $5, $6, $7)")
                    columns))
           (in-query src-conn
                     (~a "SELECT"
                         "  instance_id, study_stack, key, value, git_sha, last_put_at, first_put_at"
                         " FROM"
                         "  study_instance_data"
                         " WHERE "
                         "  instance_id = ANY($1)")
                     (list->pg-array instance-ids))))))))

(define (launch-container! name image
                           #:env [env null]
                           #:links [links null]
                           #:ports [ports (hash)]
                           #:volumes [volumes null]
                           #:network [network 'congame])
  (define data
    (hasheq
     'Image image
     'Env env
     'HostConfig (hasheq
                  'Binds volumes
                  'Links links
                  'PortBindings (for/hasheq ([(host-port container-port) (in-hash ports)])
                                  (values
                                   (string->symbol (~a container-port))
                                   (list (hasheq
                                          'HostIp "127.0.0.1"
                                          'HostPort (~a host-port))))))
     'NetworkingConfig (hasheq 'EndpointsConfig (hasheq network (hasheq)))))
  (define res
    (http:post
     (docker-uri "/containers/create")
     #:params `((name . ,name))
     #:json data))
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
