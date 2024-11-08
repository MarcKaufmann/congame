#lang racket/gui/easy

(require racket/class
         racket/string
         "cli.rkt")

(provide
 login-dialog
 ask-for-study-id-dialog)

(define (make-mix-close)
  (define close-proc! void)
  (define (mix %)
    (class %
      (super-new)
      (set! close-proc!
            (lambda ()
              (send this show #f)))))
  (values (λ () (close-proc!)) mix))

(define (login-dialog
         #:on-cancel [cancel-proc void]
         #:on-login [login-proc void])
  (define-values (close! mix-close)
    (make-mix-close))
  (define/obs @server-url "http://127.0.0.1:5100")
  (define/obs @console-log "")
  (define/obs @logging-in? #f)
  (define-values (console-in console-out)
    (make-pipe))
  (thread
   (lambda ()
     (let loop ()
       (define line (read-line console-in))
       (unless (eof-object? line)
         (@console-log . <~ . (lambda (console-log)
                                (if (equal? console-log "")
                                    line
                                    (string-append console-log "\n" line))))
         (loop)))))
  (dialog
   #:title "Congame"
   #:size '(400 #f)
   #:mixin mix-close
   (vpanel
    #:margin '(10 10)
    (hpanel
     #:stretch '(#t #f)
     (text "Server URL:")
     (input
      @server-url
      (lambda (_event text)
        (@server-url . := . text))))
    (input
     #:style '(multiple)
     #:enabled? #f
     #:min-size '(#f 100)
     @console-log)
    (hpanel
     #:alignment '(right top)
     (button
      "Cancel"
      (lambda ()
        (close!)
        (cancel-proc)))
     (button
      #:style '(border)
      #:enabled? (@logging-in? . ~> . not)
      "Log in"
      (lambda ()
        (@logging-in? . := . #t)
        (define login-thd
          (thread
           (lambda ()
             (parameterize ([current-input-port (open-input-string (string-append (obs-peek @server-url) "\n\n"))]
                            [current-output-port console-out])
               (dynamic-wind
                 void
                 (lambda () (handle-login #f))
                 (lambda () (close-output-port console-out)))
               (login-proc)
               (close!)))))
        (thread
         (lambda ()
           (sync (thread-dead-evt login-thd))
           (@logging-in? . := . #f)))))))))

(define (ask-for-study-id-dialog
         #:on-cancel [cancel-proc void]
         #:on-upload [upload-proc void])
  (define-values (close! mix-close)
    (make-mix-close))
  (define/obs @study-id "")
  (dialog
   #:title "Congame"
   #:size '(300 #f)
   #:mixin mix-close
   (vpanel
    #:margin '(10 10)
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
          (upload-proc study-id))))))))

(module+ main
  (render (ask-for-study-id-dialog))
  (render (login-dialog)))
