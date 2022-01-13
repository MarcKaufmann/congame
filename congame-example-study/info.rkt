#lang info

(define collection "congame-example-study")
(define deps '("base"
               "congame-core"
               "forms-lib"
               "koyo-lib"))
(define build-deps '())
(define congame-studies
  '((congame-example-study/example consent-study)
    (congame-example-study/example simple-study)
    (congame-example-study/looping looping-study)
    (congame-example-study/matchmaking matchmaking-study)
    (congame-example-study/multi-review review-study)))
