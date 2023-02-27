((define (foo)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "Hello world")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (bar)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "Goodbye")
        ,@(->scripts (reverse (current-study-scripts)))))))))
