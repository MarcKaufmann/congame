((define (div)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:div ((:id "bar")) (:p "Hello " (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (div-with-splitting)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:div ((:class "foo") (:style "font-weight: bold")) (:p "Hello") (:p (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (div-with-class)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:div ((:class "example")) (:p "Hello") (:p (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts)))))))))
