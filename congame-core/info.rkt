#lang info

(define collection "congame")
(define deps '("base"
               "conscript"
               "db-lib"
               "deta-lib"
               ["forms-lib" #:version "0.8"]
               "gregor-lib"
               "koyo-lib"
               ["marionette-lib" #:version "1.5"]
               "threading-lib"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
