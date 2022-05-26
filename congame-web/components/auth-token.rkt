#lang racket/base

(require db
         deta
         gregor
         koyo/database
         koyo/random
         racket/contract
         threading
         "user.rkt")

(provide
 create-auth-token!
 lookup-user-by-auth-token)

(define-schema auth-token
  #:table "auth_tokens"
  ([user-id id/f]
   [(token (generate-random-string)) string/f]
   [(expires-at (+hours (now/moment) 1)) datetime-tz/f]))

(define/contract (create-auth-token! db uid)
  (-> database? id/c string?)
  (with-database-transaction [conn db]
    (query-exec conn (~> (from auth-token #:as t)
                         (where (< t.expires-at (now)))
                         (delete)))
    (let loop ()
      (with-handlers ([exn:fail:sql:constraint-violation? (λ (_) (loop))])
        (define t (make-auth-token #:user-id uid))
        (begin0 (auth-token-token t)
          (insert-one! conn t))))))

(define/contract (lookup-user-by-auth-token db token)
  (-> database? string? (or/c #f user?))
  (with-database-transaction [conn db]
    (define maybe-user
      (lookup conn (~> (from user #:as u)
                       (join auth-token #:as t #:on (= u.id t.user-id))
                       (where (and (= t.token ,token)
                                   (> t.expires-at (now)))))))
    (begin0 maybe-user
      (when maybe-user
        (query-exec conn (~> (from auth-token #:as t)
                             (where (= t.token ,token))
                             (delete)))))))
