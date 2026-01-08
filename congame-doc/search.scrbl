#lang scribble/doc

@(require scribble/html-properties
          scribblings/main/private/utils
          scribblings/main/private/make-search)

@main-page['search #t
           #:extra-additions
           (list (make-css-addition
                  (collection-file-path
                   "search.css"
                   "scribblings/main/private")))]

@make-search[#f]
