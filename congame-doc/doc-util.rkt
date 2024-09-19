#lang racket/base

(require racket/runtime-path
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         scribble/manual)

(provide (all-defined-out))

(define-runtime-path congame-css "aux.css")
(define-runtime-path congame-tex "aux.tex")

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

(define (kbd . elems)
  (element (style "kbd" (list (css-style-addition congame-css)
                              (alt-tag "kbd")))
           elems))

(define (dr-message . elems)
  (element (style "dr-message" (list (css-style-addition congame-css)
                                     (alt-tag "span")))
           elems))

(define (browser . elems)
  (define elements
    (for/list ([elem (in-list elems)])
      (if (equal? elem "\n") (linebreak) elem)))
  (compound-paragraph 
   (style "browser" (list (css-style-addition congame-css)
                          (alt-tag "div")
                          (tex-addition congame-tex)))
   (list (paragraph (style #f null) elements))))