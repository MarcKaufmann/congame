#lang info

(define collection "congame")
(define deps '("base"
               "at-exp-lib"
               "db-lib"
               "deta-lib"
               ["forms-lib" #:version "0.6.1"]
               "gregor-lib"
               "koyo-lib"
               "marionette-lib"
               "sentry-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
