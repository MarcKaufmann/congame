#lang racket/base

(require db
         deta
         file/sha1
         gregor
         koyo/database
         koyo/hasher
         koyo/profiler
         koyo/random
         racket/contract
         racket/random
         racket/string
         racket/vector
         syntax/parse/define
         threading)

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out user)
 set-user-password
 user-enrolled-via-identity?
 user-roles-case
 user-has-roles?
 user-has-role?
 user-admin-like?
 user-researcher?
 user-anon?
 generate-api-key)

(define (non-empty-vectorof c)
  (and/c
   (vectorof c)
   (flat-named-contract
    `(non-empty-vectorof ,(contract-name c))
    (λ (v) (> (vector-length v) 0)))))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:contract non-empty-string? #:wrapper string-downcase]
   [(password-hash "") string/f]
   [api-key string/f #:nullable]
   [(roles #(user)) (array/f symbol/f) #:contract (non-empty-vectorof (or/c 'user 'bot 'api 'researcher 'admin))]
   [(verified? #f) boolean/f]
   [(verification-code (generate-random-string)) string/f #:contract non-empty-string?]
   [parent-id integer/f #:nullable]
   [bot-set-id integer/f #:nullable]
   [identity-service-url string/f #:nullable]
   [identity-service-key string/f #:nullable]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])

  #:pre-persist-hook
  (lambda (u)
    (define user-with-key
      (cond
        [(and (user-api-key u) (not (sql-null? (user-api-key u)))) u]
        [else (set-user-api-key u (generate-api-key u))]))
    (set-user-updated-at user-with-key (now/moment))))

(define-syntax-parse-rule
  (user-roles-case u
   [(role ...) e ...] ...
   [{~literal else} else-e ...])
  (cond
    [(user-has-roles? u 'role ...) e ...] ...
    [else else-e ...]))

(define/contract (user-has-roles? u . roles)
  (-> user? symbol? ... boolean?)
  (null?
   (for/fold ([roles roles])
             ([r (in-vector (user-roles u))])
     (remq r roles))))

(define/contract (user-has-role? u r)
  (-> user? symbol? boolean?)
  (and (vector-member r (user-roles u)) #t))

(define/contract (user-admin-like? u)
  (-> user? boolean?)
  (for/or ([r (in-list '(admin researcher))])
    (user-has-role? u r)))

(define/contract (user-researcher? u)
  (-> user? boolean?)
  (user-has-role? u 'researcher))

(define/contract (generate-api-key u)
  (-> user? string?)
  (bytes->hex-string
   (sha224-bytes
    (bytes-append (string->bytes/utf-8 (user-username u))
                  (crypto-random-bytes 32)))))

(define/contract (set-user-password u h p)
  (-> user? hasher? string? user?)
  (set-user-password-hash u (hasher-make-hash h p)))

(define/contract (user-password-valid? u h p)
  (-> user? hasher? string? boolean?)
  (hasher-hash-matches? h (user-password-hash u) p))

(define/contract (user-anon? u)
  (-> user? boolean?)
  (regexp-match? #rx"@anon.congame$" (user-username u)))


;; password reset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-schema password-reset
  #:table "password_reset_requests"
  ([user-id id/f #:unique]
   [ip-address string/f #:contract non-empty-string?]
   [user-agent string/f #:contract non-empty-string?]
   [(token (generate-random-string)) string/f #:contract non-empty-string?]
   [(expires-at (+days (now/moment) 1)) datetime-tz/f]))


;; user-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:fail:user-manager?
 exn:fail:user-manager:username-taken?

 make-user-manager
 user-manager?
 user-manager-hasher
 user-manager-lookup/id
 user-manager-lookup/api-key
 user-manager-lookup/username
 user-manager-create!
 user-manager-create-from-identity!
 user-manager-create-anon!
 user-manager-create-reset-token!
 user-manager-login
 user-manager-verify!
 user-manager-reset-password!)

(struct exn:fail:user-manager exn:fail ())
(struct exn:fail:user-manager:username-taken exn:fail:user-manager ())

(struct user-manager (db hasher)
  #:transparent)

(define/contract (make-user-manager db hasher)
  (-> database? hasher? user-manager?)
  (user-manager db hasher))

(define/contract (user-manager-create! um username password [roles #(user)])
  (->* (user-manager? string? string?) ((non-empty-vectorof (or/c 'admin 'api 'researcher 'bot 'user))) user?)

  (define user
    (~> (make-user #:username username
                   #:roles roles)
        (set-user-password (user-manager-hasher um) password)))

  (with-handlers ([exn:fail:sql:constraint-violation?
                   (lambda (_e)
                     (raise (exn:fail:user-manager:username-taken
                             (format "username '~a' is taken" username)
                             (current-continuation-marks))))])
    (with-database-transaction [conn (user-manager-db um)]
      (insert-one! conn user))))

(define/contract (user-manager-create-from-identity! um u username url key)
  (-> user-manager? user? string? string? string? user?)
  (define the-user
    (~> (make-user #:username username)
        (set-user-verified? #t)
        (set-user-parent-id (user-id u))
        (set-user-password (user-manager-hasher um)
                           (generate-random-string))
        (set-user-identity-service-url url)
        (set-user-identity-service-key key)))

  (with-database-connection [conn (user-manager-db um)]
    (with-handlers ([exn:fail:sql:constraint-violation?
                     (lambda (_e)
                       (lookup conn (~> (from user #:as u)
                                        (where (= u.username ,username)))))])
      (insert-one! conn the-user))))

(define/contract (user-manager-create-anon! um)
  (-> user-manager? user?)
  (define username
    (format "~a@anon.congame" (generate-random-string)))
  (define the-user
    (~> (make-user #:username username)
        (set-user-verified? #t)
        (set-user-password (user-manager-hasher um)
                           (generate-random-string))))
  (with-database-connection [conn (user-manager-db um)]
    (with-handlers ([exn:fail:sql:constraint-violation?
                     (lambda (_e)
                       (user-manager-create-anon! um))])
      (insert-one! conn the-user))))

(define/contract (user-manager-create-reset-token! um
                                                   #:username username
                                                   #:ip-address ip-address
                                                   #:user-agent user-agent)
  (-> user-manager?
      #:username non-empty-string?
      #:ip-address non-empty-string?
      #:user-agent non-empty-string?
      (values (or/c false/c user?)
              (or/c false/c string?)))
  (with-timing 'user-manager "user-manager-create-reset-token!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(user-manager-lookup/username um username)
         => (lambda (user)
              (query-exec conn (delete (~> (from password-reset #:as pr)
                                           (where (= pr.user-id ,(user-id user))))))

              (values user
                      (~> (make-password-reset #:user-id (user-id user)
                                               #:ip-address ip-address
                                               #:user-agent user-agent)
                          (insert-one! conn _)
                          (password-reset-token))))]

        [else
         (values #f #f)]))))

(define/contract (user-manager-lookup/id um id)
  (-> user-manager? exact-positive-integer? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager-lookup/id ~v)" id)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (= u.id ,id)))))))

(define/contract (user-manager-lookup/api-key um key)
  (-> user-manager? string? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager/lookup/api-key ~v)" key)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (and (array-contains? u.roles (array "api"))
                                   (= u.api-key ,key))))))))

(define/contract (user-manager-lookup/username um username)
  (-> user-manager? string? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager-lookup/username ~v)" username)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (= u.username ,(string-downcase username))))))))

(define/contract (user-manager-login um username password)
  (-> user-manager? string? string? (or/c false/c user?))
  (with-timing 'user-manager "user-manager-login"
    (define u (user-manager-lookup/username um username))
    (define h (user-manager-hasher um))
    (and u (user-password-valid? u h password) u)))

(define/contract (user-manager-verify! um id verification-code)
  (-> user-manager? exact-positive-integer? string? void?)
  (with-timing 'user-manager "user-manager-verify!"
    (void
     (with-database-transaction [conn (user-manager-db um)]
       (query-exec conn (~> (from user #:as u)
                            (update [verified? #t])
                            (where (and (= u.id ,id)
                                        (= u.verification-code ,verification-code)))))))))

(define/contract (user-manager-reset-password! um
                                               #:user-id user-id
                                               #:token token
                                               #:password password)
  (-> user-manager?
      #:user-id id/c
      #:token non-empty-string?
      #:password non-empty-string?
      boolean?)
  (with-timing 'user-manager "user-manager-reset-password!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(lookup-password-reset conn user-id token)
         => (lambda (_pr)
              (begin0 #t
                (clear-password-reset! conn user-id)
                (and~> (lookup conn
                               (~> (from user #:as u)
                                   (where (= u.id ,user-id))))
                       (set-user-password (user-manager-hasher um) password)
                       (update! conn _))))]


        [else #f]))))

(define (lookup-password-reset conn user-id token)
  (lookup conn (~> (from password-reset #:as pr)
                   (where (and (= pr.user-id ,user-id)
                               (= pr.token ,token)
                               (> pr.expires-at (now)))))))

(define (clear-password-reset! conn user-id)
  (query-exec conn (~> (from password-reset #:as pr)
                       (where (= pr.user-id ,user-id))
                       (delete))))

(define/contract (user-enrolled-via-identity? u)
  (-> user? boolean?)
  (not (sql-null? (user-identity-service-url u))))
