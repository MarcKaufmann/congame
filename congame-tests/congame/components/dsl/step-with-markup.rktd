((define (a)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "a" " " "b")
        (:p "c")
        (:h1 "Heading")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (b)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "a" " " "b")
        (:p "c")
        (:h1 "Heading")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (c)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "a" " " "b")
        (:p "c" "\n" "\n" "d")
        (:h1 "Heading")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (d)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "a" " " "b")
        (:p "c")
        (:h1 "Heading")
        (:p "d" " " "e")
        (:p "f")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (ordered-list)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:ol (:li "Test") (:li "Foo"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (unordered-list)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:ul (:li "Test") (:li "Foo"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (nested-lists)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:ul (:li (:ul (:li "Nested 1") (:li "Nested 2"))) (:li "Foo"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (anchor)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p (:a ((:href "http://example.com")) "example.com"))
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (emphasis)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "Hello " (:strong "world") "!")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (emphasis-nesting)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p (:strong "Hello " (:em "world") "!"))
        ,@(->scripts (reverse (current-study-scripts)))))))))
