#lang racket/gui/easy

(require racket/class
         (prefix-in gui: racket/gui/base)
         racket/path
         racket/string
         "gui-common.rkt"
         "wizard-draft.rkt")

(provide open-definition-wizard)

(define (open-definition-wizard root kind default-draft steps step-view validate! create-definition!
                                #:resume [resume-path #f])
  (define (merge-draft saved)
    (for/fold ([draft default-draft]) ([(key value) (in-hash (hash-ref saved 'draft))])
      (hash-set draft key value)))
  (define initial-saved (and resume-path (load-wizard-draft resume-path #:kind kind)))
  (define/obs @draft (if initial-saved (merge-draft initial-saved) default-draft))
  (define/obs @step (if initial-saved (hash-ref initial-saved 'step) 0))
  (define/obs @draft-path resume-path)
  (define/obs @status "")
  (define/obs @busy? #f)
  (define/obs @created? #f)
  (define @editable? (obs-combine (lambda (busy? created?) (not (or busy? created?))) @busy? @created?))
  (define dirty? #f)
  (define (update! key value)
    (set! dirty? #t)
    (obs-update! @draft (lambda (draft) (hash-set draft key value))))
  (define (save-draft! #:as? [as? #f])
    (with-handlers ([exn:fail? (lambda (e) (obs-set! @status (exn-message e)) #f)])
      (define current-path (obs-peek @draft-path))
      (define drafts (wizard-drafts-root root kind))
      (define directory
        (cond
          [current-path (path-only (path->complete-path current-path))]
          [(directory-exists? drafts) drafts]
          [else root]))
      (define path
        (if (and current-path (not as?))
            current-path
            (gui:put-file (format "Save ~a draft" kind) #f directory
                          (if current-path (file-name-from-path current-path) "draft.json")
                          "json")))
      (and path
           (begin
             (save-wizard-draft! root (obs-peek @draft) (obs-peek @step) #:path path #:kind kind)
             (obs-set! @draft-path path)
             (set! dirty? #f)
             (obs-set! @status (format "Draft saved: ~a" path))
             #t))))
  (define (open-draft!)
    (when (or (not dirty?)
              (= 1 (gui:message-box/custom
                    (format "Open ~a draft" kind) "Discard unsaved changes and open another draft?"
                    "Open draft" "Keep editing" #f #f '(caution default=2))))
      (with-handlers ([exn:fail? (lambda (e) (obs-set! @status (exn-message e)))])
        (define directory (wizard-drafts-root root kind))
        (define path (gui:get-file (format "Open ~a draft" kind) #f
                                   (and (directory-exists? directory) directory)))
        (when path
          (define saved (load-wizard-draft path #:kind kind))
          (obs-set! @draft (merge-draft saved))
          (obs-set! @step (hash-ref saved 'step))
          (obs-set! @draft-path path)
          (set! dirty? #f)
          (obs-set! @status "Draft opened.")))))
  (define (field key label #:height [height #f])
    (vpanel
     #:alignment '(left top) #:spacing 5 #:stretch (list #t (and height #t))
     (text label #:font section-font)
     (input (obs-map @draft (lambda (draft) (hash-ref draft key)))
            (lambda (event value)
              (when (eq? event 'input) (update! key value)))
            #:enabled? @editable?
            #:style (if height '(multiple) '(single))
            #:min-size (list 400 height)
            #:stretch (list #t (and height #t)))))
  (define (picker key label directory?)
    (hpanel
     #:stretch '(#t #f) #:spacing 8
     (field key label)
     (button "Browse…"
             (lambda ()
               (define selected
                 (if directory? (gui:get-directory label) (gui:get-file label)))
               (when selected (update! key (path->string selected))))
             #:enabled? @editable?)))
  (define (move! step)
    (with-handlers ([exn:fail? (lambda (e) (obs-set! @status (exn-message e)))])
      (when (= step (sub1 (length steps))) (validate! root (obs-peek @draft)))
      (obs-set! @status "")
      (obs-set! @step step)))
  (define (create!)
    (define draft (obs-peek @draft))
    (obs-set! @busy? #t)
    (obs-set! @status (format "Creating ~a…" kind))
    (thread
     (lambda ()
       (define result
         (with-handlers ([exn:fail? values])
           (create-definition! root draft)))
       (gui:queue-callback
        (lambda ()
          (obs-set! @busy? #f)
          (cond
            [(exn:fail? result) (obs-set! @status (exn-message result))]
            [else
             (set! dirty? #f)
             (obs-set! @created? #t)
             (obs-set! @status (format "Created ~a" result))]))))))
  (define (window-mixin %)
    (class %
      (super-new)
      (define/augment (can-close?)
        (and (not (obs-peek @busy?))
             (or (not dirty?)
                 (case (gui:message-box/custom
                        (format "Unsaved ~a" kind) "Save this draft to resume it later?"
                        "Save draft" "Discard" "Keep editing" this '(caution default=1))
                   [(1) (save-draft!)]
                   [(2) #t]
                   [else #f]))))))
  (render
   (window
    #:title (format "New benchmark ~a" kind) #:size '(850 720) #:mixin window-mixin
    (menu-bar
     (menu
      "&File"
      (menu-item (obs-map @draft-path (lambda (path) (if path "&Save" "&Save...")))
                 (lambda () (save-draft!) (void))
                 #:enabled? @editable?
                 #:shortcut (if (eq? (system-type 'os) 'macosx) '(cmd #\s) '(ctl #\s)))
      (menu-item "Save &As..." (lambda () (save-draft! #:as? #t) (void))
                 #:enabled? @editable?
                 #:shortcut (if (eq? (system-type 'os) 'macosx) '(cmd shift #\s) '(ctl shift #\s)))
      (menu-item "&Open..." open-draft!
                 #:enabled? @editable?
                 #:shortcut (if (eq? (system-type 'os) 'macosx) '(cmd #\o) '(ctl #\o)))))
    (vpanel
     #:margin '(20 18) #:spacing 16
     (hpanel
      #:stretch '(#t #f)
      (text (format "New ~a" kind) #:font heading-font)
      (spacer)
      (text (obs-map @step (lambda (step) (format "~a / ~a · ~a" (add1 step) (length steps) (list-ref steps step))))
            #:color muted))
     (observable-view @step (lambda (step) (step-view step @draft @editable? update! field picker)))
     (if-view (obs-map @status non-empty-string?)
              (reader @status #:height 60 #:stretch '(#t #f))
              (hpanel #:stretch '(#t #f)))
     (hpanel
      #:stretch '(#t #f)
      (button "Back" (lambda () (move! (sub1 (obs-peek @step))))
              #:enabled? (obs-combine (lambda (step editable?) (and (positive? step) editable?)) @step @editable?))
      (spacer)
      (if-view (obs-map @step (lambda (step) (= step (sub1 (length steps)))))
               (button (format "Create ~a" kind) create! #:style '(border) #:enabled? @editable?)
               (button "Continue →" (lambda () (move! (add1 (obs-peek @step))))
                       #:style '(border) #:enabled? @editable?)))))))
