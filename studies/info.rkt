#lang info

(define collection "studies")
(define deps
  '("base"
    "component-lib"
    "congame-core"
    "congame-web"
    "csv-reading"
    "forms-lib"
    "gregor-lib"
    "htdp-lib"
    "koyo-lib"
    "marionette-lib"
    "sentry-lib"
    "threading-lib"
    "web-server-lib"))
(define build-deps
  '("at-exp-lib"))
(define congame-studies
  '((studies/EMBA/src src-survey)
    (studies/excuses/edpb edpb-intro)
    (studies/excuses/edpb edpb-main)
    (studies/excuses/edpb edpb-pilot)
    (studies/excuses/reasons-pilot edpb-reasons-pilot)
    (studies/new-CEU-programme/pre-survey pre-survey)
    (studies/real-effort/tasks task-study)
    (studies/student-tracker/grade-expectations grade-expectations)))
