((define (form-step)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (formular
         (haml
          (:div
           (:h1 "Section 1")
           (#:name (input-text (haml "What is your name?")))
           (:h1 "Section 2")
           (#:age (input-number #:min 1 #:max 100 (haml "How old are you?")))
           (:button.button.next-button ((:type "submit")) "Submit")))
         (make-put-all-keywords void))
        ,@(->scripts (reverse (current-study-scripts)))))))))
