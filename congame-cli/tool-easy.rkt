#lang racket/gui/easy

(require racket/class
         racket/string)

(provide
 (all-defined-out))

(define (make-mix-close)
  (define close-proc! void)
  (define (mix %)
    (class %
      (super-new)
      (set! close-proc!
            (lambda ()
              (send this show #f)))))
  (values (λ () (close-proc!)) mix))

(define (ask-for-study-id-dialog
         #:on-cancel cancel-proc
         #:on-upload upload-proc)
  (define-values (close! mix-close)
    (make-mix-close))
  (define/obs @study-id "")
  (dialog
   #:title "Congame"
   #:size '(300 #f)
   #:mixin mix-close
   (hpanel
    #:stretch '(#t #f)
    (text "Study ID:")
    (input
     @study-id
     (lambda (_event text)
       (@study-id . := . text))))
   (hpanel
    #:alignment '(right top)
    (button
     "Cancel"
     (lambda ()
       (close!)
       (cancel-proc)))
    (button
     #:style '(border)
     "Upload"
     (lambda ()
       (define study-id (obs-peek @study-id))
       (unless (string=? (string-trim study-id) "")
         (close!)
         (upload-proc study-id)))))))

(module+ main
  (render
   (ask-for-study-id-dialog
    #:on-cancel void
    #:on-upload void)))
