#lang info

(define collection "congame-example-study")
(define deps '("base"
               "congame-core"
               "congame-web"
               "gregor-lib"
               "forms-lib"
               "koyo-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((congame-example-study/example consent-study)
    (congame-example-study/example simple-study)
    (congame-example-study/looping looping-study)
    (congame-example-study/matchmaking matchmaking-study)
    (congame-example-study/multi-review submit+review-pdf)
    (congame-example-study/multi-review submit+review-research-ideas)
    (congame-example-study/multi-review submit+review-pdf/intro-R)
    (congame-example-study/quizzes info-econ-quiz1)
    (congame-example-study/instructor-review instructor-review/study)))
