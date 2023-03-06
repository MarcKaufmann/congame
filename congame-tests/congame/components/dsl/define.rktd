((interpret '(define x 42) *env*)
 (define (list)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "x = " (interpret '(~a x) *env*))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (done)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "You're done.")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define hello-study
   (make-study
    "hello-study"
    #:transitions
    (transition-graph (list --> done) (done --> done))
    (list (->step 'list list) (->step 'done done)))))
