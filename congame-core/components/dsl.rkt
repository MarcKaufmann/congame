#lang racket/base

(require file/unzip
         racket/file
         racket/format
         racket/lazy-require
         racket/match
         racket/port
         racket/runtime-path
         racket/string)

(lazy-require
 [congame-web/components/upload (call-with-uploaded-file)])

(provide
 dsl-require)

(define-logger dsl)

;; XXX: Introduce dependency on conscript package without requiring it
;; until it's needed.
(define-runtime-module-path _conscript
  conscript)

(define (dsl-require src id [owner-is-admin? #f])
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
         (dsl-require* data id owner-is-admin?)))]
    [_
     (dsl-require* src id owner-is-admin?)]))

(define (dsl-require* src id owner-is-admin?)
  (log-dsl-debug "dsl-require: ~a" (~.s #:max-width 1024 src))
  (define allowed-langs
    (if owner-is-admin?
        '(conscript conscript/with-require)
        '(conscript)))
  (match-define (list _ (app (compose1 string->symbol string-trim) src-lang))
    (regexp-match #rx"^#lang ([^ \r\n]+)" src))
  (unless (memq src-lang allowed-langs)
    (error 'dsl-require "#lang may only be one of: ~a" (string-join (map ~a allowed-langs) ", ")))
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
