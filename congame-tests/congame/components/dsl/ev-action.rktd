((environment-set! *env* 'format (study-mod-require 'racket/base 'format))
 (define (test)
   (let ((*env* (make-environment *env*)))
     (interpret '(begin (put 'counter1 3) (put 'counter2 (format "~a" "Hello")) (put 'sym 'test)) *env*))))
