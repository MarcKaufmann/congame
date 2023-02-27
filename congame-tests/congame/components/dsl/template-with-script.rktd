((define (t content-proc)
   (let ((*env* (make-environment *env*))) (haml (:div (:script (string-append "console.log(\"hello from template\")"))))))
 (define (hello)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (t (λ () (error 'template "yielded without content")))
        ,@(->scripts (reverse (current-study-scripts)))))))))
