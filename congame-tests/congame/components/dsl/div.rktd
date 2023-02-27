((define (div)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:div ((:data-ignored "")) (:p "Hello " (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (div-with-splitting)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:div ((:data-ignored "")) (:p "Hello") (:p (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (div-with-class)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:div ((:data-ignored "") (:class "example")) (:p "Hello") (:p (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts)))))))))
