#lang info

(define collection "congame-web")
(define deps '("actor-lib"
               "base"
               "component-lib"
               "congame-core"
               "db-lib"
               "dbg"
               "deta-lib"
               ["forms-lib" #:version "0.8"]
               "gregor-lib"
               "hash-view-lib"
               "http-easy-lib"
               ["koyo-lib" #:version "0.23.1"]
               "koyo-north"
               "koyo-postmark"
               ["koyo-sentry" #:version "0.5.1"]
               ("libargon2-x86_64-linux" #:platform #rx"x86_64-linux")
               ("libargon2-x86_64-macosx" #:platform #rx"x86_64-macosx")
               "monocle-lib"
               "sentry-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
