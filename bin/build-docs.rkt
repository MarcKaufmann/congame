#lang racket/base

(require racket/runtime-path
         (prefix-in html: scribble/html-render)
         scribble/render
         scribble/xref)

(define-runtime-path dest
  "../build/docs")
(define-runtime-path congame.scrbl
  "../congame-doc/congame.scrbl")

(define xrefs
  (for/list ([mod+id (in-list `((setup/xref . load-collections-xref)))])
    (let* ([get-xref (dynamic-require (car mod+id) (cdr mod+id))]
           [xr (get-xref)])
      (unless (xref? xr)
        (raise-user-error
         'scribble "result from `~s' of `~s' is not an xref: ~e"
         (cdr mod+id) (car mod+id) xr))
      xr)))

(define multi-html:render-mixin
  (lambda (%)
    (html:render-multi-mixin
     (html:render-mixin %))))

(define (build-docs docs files)
  (render
   docs
   (for/list ([fn (in-list files)])
     (define-values (_base name _dir?)
       (split-path fn))
     name)
   #:dest-dir dest
   #:render-mixin multi-html:render-mixin
   #:redirect "https://docs.racket-lang.org/local-redirect/index.html"
   #:xrefs xrefs))

(module+ main
  (build-docs
   (list (dynamic-require `(file ,(path->string congame.scrbl)) 'doc))
   (list (path->string congame.scrbl))))
