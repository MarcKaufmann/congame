#lang racket/base

(require file/unzip
         racket/file
         racket/format
         racket/lazy-require
         racket/match
         racket/path
         racket/runtime-path
         racket/string)

(lazy-require
 [congame-web/components/upload (call-with-uploaded-file)])

(provide
 dsl-require)

(define-logger dsl)

;; XXX: Introduce a dependency on conscript package without requiring it
;; until it's needed.
(define-runtime-module-path _conscript
  conscript)

(define (dsl-require src id [owner-is-admin? #f])
  (match src
    [`(archive ,path)
     (call-with-uploaded-file path
       (lambda (in)
         (call-with-unzip in
           (lambda (dir)
             (define study.rkt
               (for/first ([subpath (in-directory dir)]
                           #:do [(define name (file-name-from-path subpath))]
                           #:when (equal? name (string->path "study.rkt")))
                 subpath))
             (unless study.rkt
               (error 'dsl-require "no study.rkt found in zip file"))
             (parameterize ([current-directory dir])
               (dsl-require* study.rkt id owner-is-admin?))))))]
    [_
     (call-with-temporary-file
      (lambda (path)
        (call-with-output-file path
          #:exists 'truncate/replace
          (lambda (out)
            (display src out)))
        (dsl-require* path id owner-is-admin?)))]))

(define (dsl-require* path id owner-is-admin?)
  (log-dsl-debug "dsl-require: ~.s" path)
  (define allowed-langs
    (if owner-is-admin?
        '(conscript conscript/with-require)
        '(conscript)))
  (define first-line
    (call-with-input-file path
      read-line))
  (match-define (list _ (app (compose1 string->symbol string-trim) src-lang))
    (regexp-match #rx"^#lang ([^ \r\n]+)" first-line))
  (unless (memq src-lang allowed-langs)
    (error 'dsl-require "#lang may only be one of: ~a" (string-join (map ~a allowed-langs) ", ")))
  (begin0 (dynamic-require `(file ,(path->string path)) id)
    (module-cache-clear!)
    (collect-garbage)))

(define (call-with-temporary-file proc)
  (define path #f)
  (dynamic-wind
    (lambda ()
      (set! path (make-temporary-file "conscript-dsl-~a.rkt")))
    (lambda ()
      (proc path))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file path)))))
