((define (a) (let ((*env* (make-environment *env*))) (page (haml (.container (:h1 "Hello, world!") (:p "How's it going?" " " "Pretty good?") (:p "Yeah, good.")))))))
