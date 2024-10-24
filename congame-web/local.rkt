#lang racket/base

#| This module sets up an admin account on first run for a
   docker-compose-based local setup. Relies on the fact that (start)
   sets (current-system) to prod-system. |#

(require component
         "components/user.rkt")

(provide
 setup)

(define (setup)
  (with-handlers ([exn:fail:user-manager:username-taken? void])
    (define users (system-ref 'users))
    (define the-user
      (user-manager-create!
       users
       "admin@congame.local"
       "admin"
       #(admin)))
    (user-manager-verify!
     users
     (user-id the-user)
     (user-verification-code the-user))))
