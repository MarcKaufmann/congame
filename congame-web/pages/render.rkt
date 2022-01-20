#lang racket/base

(require congame/components/export
         congame/components/study)

(provide
 study-instance->jsexpr)

;; XXX: n+1 queries here.  Refactor to use a single query if this is ever a problem.
(define (study-instance->jsexpr db study-id study-instance-id instance-vars participants)
  (hash
   'study-id study-id
   'instance-id study-instance-id
   'instance-vars (map ->jsexpr instance-vars)
   'participants
   (for/list ([p (in-list participants)])
     (define pid (study-participant/admin-id p))
     (hash
      'participant-id pid
      'instance-id study-instance-id
      'study-id study-id
      'vars (map ->jsexpr (lookup-study-vars db pid))))))
