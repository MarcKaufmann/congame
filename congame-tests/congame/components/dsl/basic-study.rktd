((define (hello)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:h1 "Hello!")
        (button void "Continue...")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (done)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "You're done!")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define hello-study
   (make-study
    "hello-study"
    #:transitions
    (transition-graph (hello --> done) (done --> done))
    (list (->step 'hello hello) (->step 'done done)))))
