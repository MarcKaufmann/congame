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

(define simulation-thd
  #f)

(define upload-button-mixin
  (mixin (drracket:unit:frame<%>) ()
    (super-new)
    (inherit get-button-panel
             get-current-tab
             get-definitions-text
             register-toolbar-button)

    (define (save-message-box)
      (message-box
       #;title "Congame"
       #;message "Please save your file first."
       #;parent #f
       #;style '(ok caution)))

    (define (get-study-id filename)
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
      (begin0 study-id
        (unless study-id
          (hash-remove! cache filename))))

    (define upload-btn
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
                                     (get-study-id filename))
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
                          (save-message-box)]))]
           [parent (get-button-panel)]
           [bitmap btn-bitmap]))
    (register-toolbar-button upload-btn)

    (define simulate-btn
      (new switchable-button%
           [label "Simulate"]
           [callback (λ (_self)
                       (define defs
                         (get-definitions-text))
                       (cond
                         [(send defs get-filename)
                          => (lambda (filename)
                               (define study-id
                                 (get-study-id filename))
                               (when study-id
                                 (when simulation-thd
                                   (break-thread simulation-thd)
                                   (thread-wait simulation-thd))
                                 (set! simulation-thd
                                       (thread
                                        (lambda ()
                                          (simulate "http://127.0.0.1:5100" 2 study-id))))))]
                         [else
                          (save-message-box)]))]
           [parent (get-button-panel)]
           [bitmap btn-bitmap]))
    (register-toolbar-button simulate-btn)

    (send
     (get-button-panel)
     change-children
     (lambda (btns)
       (cons simulate-btn (cons upload-btn (remq simulate-btn (remq upload-btn btns))))))))

(define reset-menu-item-mixin
  (mixin (drracket:unit:frame<%>) ()
    (super-new)
    (inherit get-definitions-text
             get-language-menu)

    (define menu
      (get-language-menu))
    (new separator-menu-item%
         [parent menu])
    (new menu-item%
         [parent menu]
         [label "Conscript: Reset Study"]
         [callback (lambda (_self _event)
                     (define defs (get-definitions-text))
                     (define filename (send defs get-filename))
                     (when filename (hash-remove! cache filename)))])))

(define btn-bitmap
  (call-with-input-file upload.png
    (lambda (in)
      (read-bitmap
       #:backing-scale 2.0
       in 'png/alpha))))

(drracket:get/extend:extend-unit-frame upload-button-mixin)
(drracket:get/extend:extend-unit-frame reset-menu-item-mixin)
