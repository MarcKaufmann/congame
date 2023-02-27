((environment-set! *env* 'println (study-mod-require 'racket/base 'println))
 (define (quit) (let ((*env* (make-environment *env*))) (interpret '(println "hello") *env*)))
 (define (hello)
   (let ((*env* (make-environment *env*)))
     (page
      (haml
       (.container
        ,@(->styles (reverse (current-study-styles)))
        (button quit "Quit")
        ,@(->scripts (reverse (current-study-scripts)))))))))
