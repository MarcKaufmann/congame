((define (a)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:h1 "Step a")
        (button #:to-step-id 'c void "Next")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (b)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:h1 "Step b")
        (button void "Next")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (c)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:h1 "Step c")
        (button void "Next")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define s
   (make-study "s" #:transitions (transition-graph (a --> b --> c) (c --> c)) (list (->step 'a a) (->step 'b b) (->step 'c c)))))
