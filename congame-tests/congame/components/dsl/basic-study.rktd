((define (hello) (let ((*env* (make-environment *env*))) (page (haml (.container (:h1 "Hello!") (button void "Continue..."))))))
 (define (done) (let ((*env* (make-environment *env*))) (page (haml (.container (:p "You're done!"))))))
 (define hello-study (make-study "hello-study" #:transitions (transition-graph (hello --> done) (done --> done)) (list (->step 'hello hello) (->step 'done done)))))
