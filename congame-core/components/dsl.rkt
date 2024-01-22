#lang racket/base

(require racket/file
         racket/format)

(provide
 dsl-require)

(define-logger dsl)

(define (dsl-require src id)
  (log-dsl-debug "dsl-require: ~a" (~.s #:max-width 1024 src))
  (unless (regexp-match? #rx"^#lang conscript *\n" src)
    (error 'dsl-require "only #lang conscript is supported"))
  (define path #f)
  (dynamic-wind
    (lambda ()
      (set! path (make-temporary-file "conscript-dsl-~a.rkt")))
    (lambda ()
      (call-with-output-file path
        #:exists 'truncate/replace
        (lambda (out)
          (display src out)))
      (begin0 (dynamic-require `(file ,(path->string path)) id)
        (module-cache-clear!)
        (collect-garbage)))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file path)))))
