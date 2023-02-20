((define (pre-step-foo) (with-study-transaction (let ((*env* (make-environment *env*))) (interpret '(put 'x 42) *env*))))
 (define (foo) (let ((*env* (make-environment *env*))) (pre-step-foo) (page (haml (.container (:p "Hello world")))))))
