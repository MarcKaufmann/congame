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
                     "koyo-lib"
                     "koyo-north"
                     "threading-lib"
                     "rackunit-lib"))

(define update-implies '("congame-web"))

(define congame-studies
  '((tests/congame/studies/test-substudy-failing test-substudy-failing)))
