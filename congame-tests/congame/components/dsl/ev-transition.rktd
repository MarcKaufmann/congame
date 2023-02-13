((define (step0) (let ((*env* (make-environment *env*))) (page (haml (.container (button void "Continue"))))))
 (define (step1) (let ((*env* (make-environment *env*))) (page (haml (.container (button void "Continue"))))))
 (define (step2) (let ((*env* (make-environment *env*))) (page (haml (.container (button void "Continue"))))))
 (define (done) (let ((*env* (make-environment *env*))) (page (haml (.container (:p "You're done."))))))
 (define hello-study
   (make-study
    "hello-study"
    #:transitions
    (transition-graph (step0 --> ,(interpret '(lambda () (cond ((= (get 'some-var) "agree") (goto step1)) ((= (get/instance 'some-other-var) "always-agree") (goto step1)) (else (goto step2)))) *env*)) (step1 --> done) (step2 --> done) (done --> done))
    (list (->step 'step0 step0) (->step 'step1 step1) (->step 'done done) (->step 'step2 step2)))))
