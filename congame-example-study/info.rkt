#lang info

(define collection "congame-example-study")
(define deps '("base"
               "component-lib"
               "congame-core"
               "congame-web"
               "gregor-lib"
               "forms-lib"
               "koyo-lib"
               "sentry-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((congame-example-study/edpb-survey briq-study)
    (congame-example-study/edpb-survey edpb-survey)
    (congame-example-study/example consent-study)
    (congame-example-study/example simple-study)
    (congame-example-study/example wrapped-simple-study)
    (congame-example-study/looping looping-study)
    (congame-example-study/matchmaking matchmaking-study)
    (congame-example-study/multi-review submit+review-pdf)
    (congame-example-study/multi-review submit+review-research-ideas)
    (congame-example-study/multi-review submit+review-pdf/intro-R)
    (congame-example-study/multi-review submit+review-pdf/intro-R-exercises)
    (congame-example-study/multi-review submit+review-pdf/beliefs)
    (congame-example-study/quizzes info-econ-quiz1)
    (congame-example-study/quizzes info-econ-quiz2)
    (congame-example-study/quizzes info-econ-quiz3)
    (congame-example-study/quizzes info-econ-quiz4)
    (congame-example-study/instructor-review instructor-review/study)
    (congame-example-study/reproducible-bug reproducible-bug-study)))
