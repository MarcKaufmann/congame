#lang racket/base

(require racket/class
         racket/file
         racket/path
         racket/runtime-path
         scribble/base-render
         (prefix-in html: scribble/html-render)
         scribble/xref)

(define-runtime-path dest
  "../build/docs")
(define-runtime-path congame.scrbl
  "../congame-doc/congame.scrbl")
(define-runtime-path search.scrbl
  "../congame-doc/search.scrbl")

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

(define (build-docs docs filenames)
  (make-directory* dest)
  (define renderer
    (new (multi-html:render-mixin render%)
         [dest-dir dest]
         [style-file (collection-file-path "manual-style.css" "scribble")]
         [extra-files (list (collection-file-path "manual-fonts.css" "scribble"))]
         [search-box? #t]
         [search-up-path #t]))
  (send renderer report-output!)
  (send renderer set-external-tag-path "https://docs.racket-lang.org/local-redirect/index.html")
  (send renderer set-external-root-url "https://docs.racket-lang.org")
  (define fp (send renderer traverse docs filenames))
  (define ci (send renderer collect docs filenames fp))
  (for ([xr (in-list xrefs)])
    (xref-transfer-info renderer ci xr))
  (define ri (send renderer resolve docs filenames ci))
  (send renderer render docs filenames ri)
  (void))

(module+ main
  (build-docs
   (list
    (dynamic-require `(file ,(path->string congame.scrbl)) 'doc)
    (dynamic-require `(file ,(path->string search.scrbl)) 'doc))
   (list
    (build-path dest "congame")
    (build-path dest "search")))
  (for ([path (in-directory (build-path dest "congame"))]
        #:when (member (path-get-extension path) '(#".css" #".js")))
    (define-values (_dir name _is-dir?)
      (split-path path))
    (copy-file #:exists-ok? #t path (build-path dest "search" name))))
