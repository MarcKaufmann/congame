((define (list)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "Test")
        (:ul ,@(interpret '(list "a" "b" "c") *env*))
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
