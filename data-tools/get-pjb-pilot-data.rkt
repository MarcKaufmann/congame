#lang racket/base

(require "get-data.rkt")

(define api-url "https://totalinsightmanagement.com/api/v1/")
(define data-folder "/home/marc/Git/kutat/projection-bias-experiment/data/")

(define pjb-pilot-study-id 1)
; one of the instances was a pure pilot, so I drop it
(define pjb-pilot-iids
  '(2 3 4 5 7 9))

(define vars-to-keep
  '(study-id
    instance-id
    participant-id
    completed?
    practice-tasks
    tutorial-fee
    required-tasks
    participation-fee
    relax-treatment
    tutorial-success?
    consent?
    rest-treatment
    (relaxation-survey classical-piano-motivating-score)
    (relaxation-survey classical-piano-relaxing-score)
    (relaxation-survey edm-motivating-score)
    (relaxation-survey edm-relaxing-score)
    (relaxation-survey guided-meditation-motivating-score)
    (relaxation-survey guided-meditation-relaxing-score)
    (relaxation-survey wave-sounds-motivating-score)
    (relaxation-survey wave-sounds-relaxing-score)
    (relaxation-survey own-track)
    (WTWs pl5)
    (WTWs pl8)
    (WTWs pl11)
    (WTWs pl15)
    (WTWs pl25)
    (debrief-survey comments)
    (debrief-survey gender)
    (debrief-survey how-clear-were-instructions)
    (debrief-survey how-do-you-decide-on-extra-work)
    (debrief-survey how-relaxing)
    (debrief-survey other-restful-activity)
    (debrief-survey what-could-be-clearer)
    (debrief-survey work-factors fewer-extra-tasks)
    (debrief-survey work-factors have-more-time)
    (debrief-survey work-factors longer-break)
    (debrief-survey work-factors smaller-matrices)
    ))

(module+ main
  (get-and-write-data #:study-id pjb-pilot-study-id
                      #:iids pjb-pilot-iids
                      #:api-url api-url
                      #:vars-to-keep vars-to-keep
                      #:path (build-path data-folder "pjb-pilot.csv")))
