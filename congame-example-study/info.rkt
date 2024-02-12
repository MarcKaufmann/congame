#lang info

(define collection "congame-example-study")
(define deps
  '("base"
    "component-lib"
    "congame-core"
    "congame-web"
    "conscript"
    "gregor-lib"
    "forms-lib"
    "koyo-lib"
    "sentry-lib"
    "threading-lib"
    "web-server-lib"))
(define build-deps '())
(define congame-studies
  '((congame-example-study/calibration calibration)
    (congame-example-study/conscript conscript-example)
    (congame-example-study/conscript-bot conscript-bot-example)
    (congame-example-study/conscript-defvar conscript-defvar-example)
    (congame-example-study/conscript-for-study conscript-for-study-example)
    (congame-example-study/conscript-form conscript-form-example)
    (congame-example-study/conscript-form-inline conscript-form-inline-example)
    (congame-example-study/conscript-markdown conscript-markdown-example)
    (congame-example-study/conscript-reqprovide conscript-reqprovide-example)
    (congame-example-study/deferred deferred-study)
    (congame-example-study/deferred wrapped-deferred-study)
    (congame-example-study/edpb-survey briq-study)
    (congame-example-study/edpb-survey edpb-survey)
    (congame-example-study/example consent-study)
    (congame-example-study/example simple-study)
    (congame-example-study/example wrapped-simple-study)
    (congame-example-study/be24 reading1-quiz)
    (congame-example-study/inline inline-study)
    (congame-example-study/instructor-review instructor-review/study)
    (congame-example-study/looping looping-study)
    (congame-example-study/looping nested-looping-study)
    (congame-example-study/memory-leak memory-leak)
    (congame-example-study/matchmaking matchmaking-study)
    (congame-example-study/multi-review submit+review-pdf)
    (congame-example-study/multi-review submit+review-pdf/beliefs)
    (congame-example-study/multi-review submit+review-pdf/intro-R)
    (congame-example-study/multi-review submit+review-pdf/intro-R-exercises)
    (congame-example-study/multi-review submit+review-research-ideas)
    (congame-example-study/prolific-redirect prolific-redirect)
    (congame-example-study/quizzes info-econ-quiz1)
    (congame-example-study/quizzes info-econ-quiz2)
    (congame-example-study/quizzes info-econ-quiz3)
    (congame-example-study/quizzes info-econ-quiz4)
    (congame-example-study/reproducible-bug reproducible-bug-study)))
(define congame-bots
  '((congame-example-study/memory-leak make-ml-bot #:for memory-leak #:models (ml-bot-model))
    (congame-example-study/conscript-bot make-conscript-bot #:for conscript-bot-example #:models (conscript-bot-model))))
