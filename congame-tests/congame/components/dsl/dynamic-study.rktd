((environment-set! *env* 'make-tasks (study-mod-require 'somewhere 'make-tasks))
 (define tasks-study (interpret '(make-tasks 5) *env*))
 (define hello-study
   (make-study
    "hello-study"
    #:transitions
    (transition-graph (tasks-study --> done) (done --> done))
    (list (->step 'tasks-study tasks-study) (->step 'done done)))))
