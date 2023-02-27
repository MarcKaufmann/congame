((define (a)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:h1 "Hello, world!")
        (:p "How's it going?" " " "Pretty good?")
        (:p "Yeah, good.")
        ,@(->scripts (reverse (current-study-scripts)))))))))
