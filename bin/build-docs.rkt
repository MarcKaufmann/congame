#lang racket/base

(require racket/class
         racket/file
         racket/runtime-path
         scribble/base-render
         (prefix-in html: scribble/html-render)
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
  (make-directory* dest)
  (define renderer
    (new (multi-html:render-mixin render%)
         [dest-dir dest]
         [search-box? #t]))
  (send renderer report-output!)
  (send renderer set-external-tag-path "https://docs.racket-lang.org/local-redirect/index.html")
  (define suffix
    (send renderer get-suffix))
  (define filenames
    (for/list ([fn (in-list files)])
      (define-values (_base name _dir?)
        (split-path fn))
      (build-path dest (path-replace-suffix name suffix))))
  (define fp (send renderer traverse docs filenames))
  (define ci (send renderer collect docs filenames fp))
  (for ([xr (in-list xrefs)])
    (xref-transfer-info renderer ci xr))
  (define ri (send renderer resolve docs filenames ci))
  (send renderer render docs filenames ri)
  (void))

(module+ main
  (build-docs
   (list (dynamic-require `(file ,(path->string congame.scrbl)) 'doc))
   (list (path->string congame.scrbl))))
