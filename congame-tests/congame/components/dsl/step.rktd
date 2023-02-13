((define (a) (let ((*env* (make-environment *env*))) (page (haml (.container (:p "a" " " "b") (:p "c")))))))
