#lang info

(define collection "studies")
(define deps '("base"
               "component-lib"
               "congame-core"
               "congame-web"
               "forms-lib"
               "gregor-lib"
               "koyo-lib"
               "marionette-lib"
               "sentry-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((studies/new-CEU-programme/pre-survey pre-survey)))
