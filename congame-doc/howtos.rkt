#lang racket/base

;; Stolen from:
;;   https://github.com/racket/racket/blob/65631ed7d1548179914068110dca54d1fea552de/pkgs/racket-doc/scribblings/style/shared.rkt

(require racket/list
         racket/match
         (only-in scribble/core table-columns table-cells style plain
                  color-property nested-flow)
         scribble/html-properties
         scribble/manual
         scribble/struct)

(provide
 compare0)

(define stretching-style
  (style #f (list (attributes '([style . "margin-left: 0; margin-right: 0"])))))

(define (stretch d)
  (match d
    [(nested-flow _ content) (nested-flow stretching-style content)]
    [_ d]))

;; compare0: two code snippets, in two columns: left is good, right is bad
(define (compare0 #:left [left "good"] #:right [right "bad"]
                  stuff1 stuff2)
  (define stuff (list (list (stretch (filebox (tt left) stuff1)))
                      (list (stretch (filebox (tt right) stuff2)))))
  (table (sty 2 500) (apply map (compose make-flow list) stuff)))


(define (sty columns width #:valign? [valign? #t])
  (define space
    (style #f `(,(attributes `((width . ,(format "~a" width)) (align . "left")
                                                              ,@(if valign?
                                                                    (list '(valign . "top"))
                                                                    (list)))))))
  ;; -- in --
  (style #f
    (list
     (attributes '((border . "1") (cellpadding . "1")))
     (table-columns (make-list columns space)))))
