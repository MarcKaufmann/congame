#lang racket/gui/easy

(require racket/class
         (prefix-in gui: racket/gui/base)
         "code-style.rkt")

(provide heading-font section-font score-font muted reader)

(define heading-font (gui:make-font #:size 18 #:weight 'bold))
(define section-font (gui:make-font #:size 13 #:weight 'bold))
(define score-font (gui:make-font #:size 24 #:weight 'bold))
(define code-face
  (for/first ([face (in-list (gui:get-face-list))]
              #:when (string-ci=? face "SF Mono"))
    face))

(define (hex-color value)
  (apply gui:make-color
         (for/list ([start '(1 3 5)])
           (string->number (substring value start (+ start 2)) 16))))

(define muted (hex-color "#667085"))

(define (reader content
                #:height [height 200]
                #:stretch [stretch '(#t #t)]
                #:code? [code? #f]
                #:highlight? [highlight? #f])
  (define (make-editor value)
    (define editor (new gui:text%))
    (send editor insert value)
    (define style (new gui:style-delta%))
    (send style set-family (if code? 'modern 'system))
    (when (and code? code-face)
      (send style set-face code-face))
    (send editor change-style style 0 (send editor last-position))
    (when highlight?
      (for ([span (in-list (code-spans value))])
        (define color-style (new gui:style-delta%))
        (send color-style set-delta-foreground (hex-color (code-span-color span)))
        (send editor change-style color-style (code-span-start span) (code-span-end span))))
    (send editor auto-wrap #t)
    (send editor lock #t)
    (send editor set-position 0)
    editor)
  (editor-canvas (if (obs? content)
                     (obs-map content make-editor)
                     (make-editor content))
                 #:style '(no-hscroll no-border)
                 #:inset '(12 10)
                 #:min-size (list 300 height)
                 #:stretch stretch))

