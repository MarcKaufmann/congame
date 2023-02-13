#lang racket/base

(require racket/path
         racket/port
         racket/pretty
         racket/runtime-path
         rackunit

         congame/components/dsl)

(provide
 dsl-tests)

(define-runtime-path snapshots "dsl")

(define (read+compile str)
  (call-with-input-string str
    (lambda (in)
      (port-count-lines! in)
      (read-syntax+compile "<string>" in))))

(define dsl-tests
  (test-suite
   "dsl"

   (test-case "read-syntax+compile"
     (check-exn
      #rx"not a valid step id"
      (lambda ()
        (read+compile #<<DSL
@step[end]{Hello}
DSL
                      )))

     (check-exn
      #rx"not a valid study id"
      (lambda ()
        (read+compile #<<DSL
@study[
  end
  #:transitions
  [a --> b]]
DSL
                      )))

     (for ([in-path (in-directory snapshots)]
           #:when (path-has-extension? in-path #".scrbl"))
       (define out-path
         (path-replace-extension in-path #".rktd"))
       (define output
         (call-with-input-file in-path
           (lambda (in)
             (port-count-lines! in)
             (syntax->datum (read-syntax+compile in-path in)))))
       (cond
         [(file-exists? out-path)
          (define expected
            (call-with-input-file out-path read))
          (check-equal? output expected (path->string in-path))]
         [else
          (call-with-output-file out-path
            (lambda (out)
              (pretty-write output out)))])))))

(module+ test
  (require rackunit/text-ui)
  (run-tests dsl-tests))
