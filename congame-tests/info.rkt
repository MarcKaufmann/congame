#lang info

(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "component-lib"
                     "congame-core"
                     "congame-pjb-studies"
                     "db-lib"
                     "deta-lib"
                     "koyo-lib"
                     "koyo-north"
                     "threading-lib"
                     "rackunit-lib"

                     "congame"))

(define update-implies '("congame"))
