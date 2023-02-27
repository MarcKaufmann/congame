((define (a content-proc) (let ((*env* (make-environment *env*))) (haml (:div (:p "Hello " ,@(content-proc) "!")))))
 (define (b content-proc)
   (let ((*env* (make-environment *env*)))
     (haml (:div (a (λ () (list (haml "Hi world!")))) (a (λ () (haml "Hello" "\n" "\n" "there" "\n" "\n" (:h1 "Friend"))))))))
 (define (hello)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (b (λ () (error 'template "yielded without content")))
        ,@(->scripts (reverse (current-study-scripts)))))))))
