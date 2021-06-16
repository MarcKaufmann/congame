#lang info

(define collection "congame-web")
(define deps '("base"
               "component-lib"
               "congame-core"
               "crypto-lib"
               "db-lib"
               "deta-lib"
               ; TODO: Restore reference to forms-lib after catalog update
               "git://github.com/Bogdanp/racket-forms?path=forms-lib"
               "gregor-lib"
               "koyo-lib"
               "koyo-north"
               "koyo-postmark"
               "koyo-sentry"
               ("libargon2-x86_64-linux" #:platform #rx"x86_64-linux")
               ("libargon2-x86_64-macosx" #:platform #rx"x86_64-macosx")
               "sentry-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
