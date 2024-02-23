#lang racket/base

(require congame/components/study
         (submod congame/components/study accessors)
         racket/list)

(provide
 assigning-treatments)

(define (assigning-treatments
         treatments
         #:treatments-key [treatments-key 'treatments]
         #:role-key [role-key 'role])
  (unless (get* role-key #f)
    (with-study-transaction
      (when (null? (get/instance* treatments-key '()))
        (put/instance* treatments-key (shuffle treatments)))
      (define remaining-treatments
        (get/instance* treatments-key))
      (define role
        (car remaining-treatments))
      (put* role-key role)
      (define updated-treatments
        (cdr remaining-treatments))
      (put/instance* treatments-key updated-treatments))))
