#lang info

(define collection "studies")
(define deps
  '("base"
    "component-lib"
    "congame-core"
    "congame-web"
    "csv-reading"
    "gregor-lib"
    "koyo-lib"
    "sentry-lib"
    "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((studies/EMBA/src src-survey)
    (studies/excuses/edpb edpb-intro)
    (studies/excuses/edpb edpb-main)
    (studies/new-CEU-programme/pre-survey pre-survey)
    (studies/real-effort/tasks task-study)))
