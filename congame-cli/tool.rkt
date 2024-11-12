#lang racket/unit

(require drracket/tool
         mrlib/switchable-button
         racket/gui
         racket/gui/easy
         "asset.rkt"
         "cli.rkt"
         "tool-easy.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

(define (phase1) (void))
(define (phase2) (void))

(define cache
  (make-hash))

(define upload-button-mixin
  (mixin (drracket:unit:frame<%>) ()
    (super-new)
    (inherit get-button-panel
             get-current-tab
             get-definitions-text
             register-toolbar-button)

    (define btn
      (new switchable-button%
           [label "Upload Study"]
           [callback (λ (_self)
                       (define defs
                         (get-definitions-text))
                       (cond
                         [(send defs get-filename)
                          => (lambda (filename)
                               (let loop ([force-login? #f])
                                 (let/ec esc
                                   (unless (if force-login? #f (get-preference 'congame-cli:key))
                                     (define logged-in? #f)
                                     (define r
                                       (embed
                                        (send (get-current-tab) get-frame)
                                        (login-dialog
                                         #:on-login (λ () (set! logged-in? #t)))))
                                     (send (renderer-root r) show #t)
                                     (unless logged-in?
                                       (message-box
                                        #;title "Congame"
                                        #;message "You are not logged in."
                                        #;parent #f
                                        #;style '(ok caution))
                                       (esc)))
                                   (define study-id
                                     (hash-ref!
                                      #;ht cache
                                      #;key filename
                                      #;failure-proc
                                      (lambda ()
                                        (define res #f)
                                        (define r
                                          (embed
                                           (send (get-current-tab) get-frame)
                                           (ask-for-study-id-dialog
                                            #:on-cancel void
                                            #:on-upload (λ (id) (set! res id)))))
                                        (send (renderer-root r) show #t)
                                        res)))
                                   (when study-id
                                     (with-handlers* ([exn:fail:api:not-authorized?
                                                       (lambda (_e)
                                                         (loop #t))]
                                                      [exn:fail?
                                                       (lambda (e)
                                                         (hash-remove! cache filename)
                                                         (raise e))])
                                       (upload-study study-id filename))))))]
                         [else
                          (message-box
                           #;title "Congame"
                           #;message "Please save your file first."
                           #;parent #f
                           #;style '(ok caution))]))]
           [parent (get-button-panel)]
           [bitmap btn-bitmap]))
    (register-toolbar-button btn)
    (send (get-button-panel) change-children (λ (btns) (cons btn (remq btn btns))))))

(define btn-bitmap
  (call-with-input-file upload.png
    (lambda (in)
      (read-bitmap
       #:backing-scale 2.0
       in 'png/alpha))))

(drracket:get/extend:extend-unit-frame upload-button-mixin)
