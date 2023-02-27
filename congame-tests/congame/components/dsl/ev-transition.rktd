((define (step0)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (button void "Continue")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (step1)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (button void "Continue")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (step2)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (button void "Continue")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define (done)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (:p "You're done.")
        ,@(->scripts (reverse (current-study-scripts))))))))
 (define hello-study
   (make-study
    "hello-study"
    #:transitions
    (transition-graph
     (step0
      -->
      ,(interpret
        '(lambda ()
           (cond
            ((= (get 'some-var) "agree") (goto step1))
            ((= (get/instance 'some-other-var) "always-agree") (goto step1))
            (else (goto step2))))
        *env*))
     (step1 --> done)
     (step2 --> done)
     (done --> done))
    (list (->step 'step0 step0) (->step 'step1 step1) (->step 'done done) (->step 'step2 step2)))))
