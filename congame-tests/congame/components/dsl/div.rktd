((define (div) (let ((*env* (make-environment *env*))) (page (haml (.container (:div ((:data-ignored "")) (:p "Hello " (:em "world") "!")))))))
 (define (div-with-splitting) (let ((*env* (make-environment *env*))) (page (haml (.container (:div ((:data-ignored "")) (:p "Hello") (:p (:em "world") "!")))))))
 (define (div-with-class) (let ((*env* (make-environment *env*))) (page (haml (.container (:div ((:data-ignored "") (:class "example")) (:p "Hello") (:p (:em "world") "!"))))))))
