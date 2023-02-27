((define (a)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "a" " " "b")
        (:p "c")
        ,@(->scripts (reverse (current-study-scripts)))))))))
