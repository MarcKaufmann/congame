#lang info

(define collection "studies")
(define deps '("base"
               "component-lib"
               "congame-core"
               "congame-web"
               "csv-reading"
               "forms-lib"
               "gregor-lib"
               "koyo-lib"
               "sentry-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((studies/excuses/edpb edpb-pilot)
    (studies/excuses/abstract-categorization abstracts-admin)
    (studies/excuses/abstract-categorization abstract-tasks)))
