#lang racket/base

(require file/unzip
         racket/file
         racket/format
         racket/lazy-require
         racket/match
         racket/port)

(lazy-require
 [congame-web/components/upload (call-with-uploaded-file)])

(provide
 dsl-require)

(define-logger dsl)

(define (dsl-require src id)
  (match src
    [`(archive ,path)
     (call-with-uploaded-file path
       (lambda (in)
         (define dir
           (read-zip-directory in))
         (define entry-name
           (for/first ([e (in-list (zip-directory-entries dir))]
                       #:when (regexp-match? #rx#"/?study.rkt$" e))
             e))
         (unless entry-name
           (error 'dsl-require "no study.rkt found in zip file"))
         (define data "")
         (unzip-entry
          in dir entry-name
          (lambda (_name _dir? entry-in)
            (set! data (port->string entry-in))))
         (dsl-require* data id)))]
    [_
     (dsl-require* src id)]))

(define (dsl-require* src id)
  (log-dsl-debug "dsl-require: ~a" (~.s #:max-width 1024 src))
  (unless (or (regexp-match? #rx"^#lang conscript *\n" src)
              (regexp-match? #rx"^#lang conscript *\r\n" src))
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
