#lang info

(define collection "congame-web")
(define deps '("base"
               "component-lib"
               "congame-core"
               "conscript"
               "crypto-lib"
               "db-lib"
               "dbg"
               "deta-lib"
               "forms-lib"
               "gregor-lib"
               "hash-view-lib"
               "http-easy"
               ["koyo-lib" #:version "0.23.1"]
               "koyo-north"
               "koyo-postmark"
               ["koyo-sentry" #:version "0.1.1"]
               ("libargon2-x86_64-linux" #:platform #rx"x86_64-linux")
               ("libargon2-x86_64-macosx" #:platform #rx"x86_64-macosx")
               "sentry-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
