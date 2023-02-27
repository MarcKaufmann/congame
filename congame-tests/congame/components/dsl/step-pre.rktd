((define (pre-step-foo) (let ((*env* (make-environment *env*))) (interpret '(put 'x 42) *env*)))
 (define (foo)
   (let ((*env* (make-environment *env*)))
     (pre-step-foo)
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "Hello world")
        ,@(->scripts (reverse (current-study-scripts)))))))))
