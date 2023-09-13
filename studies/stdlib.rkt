#lang racket/base

(require congame/components/study
         (submod congame/components/study accessors))

(provide
 assigning-participant&owner)

(define (assigning-participant&owner)
  (cond [(current-participant-owner?)
         (put* 'role 'admin)
         (skip)]
        [else
         (put* 'role 'participant)
         (skip)]))
