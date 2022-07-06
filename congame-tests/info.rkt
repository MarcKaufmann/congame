#lang info

(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "component-lib"
                     "congame-core"
                     "congame-pjb-studies"
                     "congame-web"
                     "db-lib"
                     "deta-lib"
                     "http-easy"
                     "koyo-lib"
                     "koyo-north"
                     "marionette-lib"
                     "threading-lib"
                     "rackunit-lib"))

(define update-implies '("congame-web"))

(define congame-studies
  '((tests/congame/studies/test-empty-study test-empty-study)
    (tests/congame/studies/test-looping-failures test-looping-failures)
    (tests/congame/studies/test-substudy-failing test-substudy-failing)))
