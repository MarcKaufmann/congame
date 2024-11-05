#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scribble/manual)

(provide (all-defined-out))

(define-runtime-path congame-css "aux.css")
(define-runtime-path congame-tex "aux.tex")

(define at "@")
(define _at (litchar "@"))

;; Mark text as worthy of review for possible Congame improvements
(define (mark . elems)
  (element (style "review" (list (css-style-addition congame-css)
                                 (alt-tag "mark")))
           elems))

;; Mark filler content as placeholders for the real thing to be added later
(define (tktk . elems)
  (element (style "tktk" (list (css-style-addition congame-css)
                               (alt-tag "span")))
           `(,(icon "Content to be added later" "⏳") ,@elems)))

(define (icon tooltip str)
  (element (style "margin-icon" (list (attributes `([title ,@tooltip]))
                                      (alt-tag "abbr")))
           (list str)))

;; Style for sample terminal output 
(define (terminal . args)
  (compound-paragraph (style "terminal" (list (color-property (list #x66 #x33 #x99))
                                              (css-style-addition congame-css)
                                              (alt-tag "div")
                                              (tex-addition congame-tex)))
                      (list (apply verbatim args))))

;; Simulate a command-line prompt
(define (:> . elems)
  (element (style "prompt" (list (color-property (list #x66 #x66 #x66))))
           (apply exec (cons "> " elems))))

;; Simulate a bash-style comment
(define (rem . args)
  (apply racketcommentfont (cons "# " args)))

;; Style text as a keyboard key or a button
(define (kbd . elems)
  (element (style "kbd" (list (css-style-addition congame-css)
                              (alt-tag "kbd")))
           elems))

;; Style for output in the DrRacket interactions window
(define (dr-message . elems)
  (element (style "dr-message" (list (css-style-addition congame-css)
                                     (alt-tag "span")))
           elems))

;; Simulate a browser window
(define (browser . elems)
  (compound-paragraph
   (style "browser" (list (css-style-addition congame-css)
                          (alt-tag "div")
                          (tex-addition congame-tex)))
   (decode-flow elems)))

;; For use inside `browser`
(define (mock-textbox)
  (element (style "mock-textbox" (list (css-style-addition congame-css)
                                       (alt-tag "span")))
           " "))

;; Insert a screenshot, using a runtime path, centered and scaled down
(define-syntax (screenshot stx)
  (syntax-case stx ()
    [(_ name-path-str xs ...)
     (with-syntax ([name-id (datum->syntax stx (string->symbol (syntax-e #'name-path-str)))])
       #'(begin
           (define-runtime-path name-id (quote name-path-str))
           (centered
            (image-element (style "figure" (list (css-style-addition congame-css)))
                           '() name-id '() 0.4))))]))

