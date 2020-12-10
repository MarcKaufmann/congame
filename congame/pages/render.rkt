#lang racket/base

(require congame/components/export
         congame/components/study)

(provide
 study-participants->jsexpr)

;; XXX: n+1 queries here.  Refactor to use a single query if this is ever a problem.
(define (study-participants->jsexpr db study-id study-instance-id participants)
  (hash
   'study-id study-id
   'instance-id study-instance-id
   'participants
   (for/list ([p (in-list participants)])
     (define pid (study-participant/admin-id p))
     (hash
      'participant-id pid
      'instance-id study-instance-id
      'study-id study-id
      'vars (map ->jsexpr (lookup-study-vars db pid))))))
