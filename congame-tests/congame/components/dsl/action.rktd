((environment-set! *env* 'println (study-mod-require 'racket/base 'println))
 (define (quit) (with-study-transaction (let ((*env* (make-environment *env*))) (interpret '(println "hello") *env*))))
 (define (hello) (let ((*env* (make-environment *env*))) (page (haml (.container (button quit "Quit")))))))
