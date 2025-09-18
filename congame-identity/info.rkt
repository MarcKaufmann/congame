#lang info

(define collection "congame-identity")
(define deps '("base"
               "buid"
               "component-lib"
               "db-lib"
               ("deta-lib" #:version "0.9")
               "forms-lib"
               "gregor-lib"
               "http-easy"
               ["koyo-lib" #:version "0.44"]
               "koyo-north"
               "koyo-postmark"
               ["koyo-sentry" #:version "0.1.1"]
               "libargon2"
               "net-lib"
               "sentry-lib"
               "smtp-server"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
