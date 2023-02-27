((define (a)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "Hello world")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define binding-transitions
   (make-study "binding-transitions" #:transitions (transition-graph (a --> b --> b)) (list (->step 'a a) (->step 'b a)))))
