#lang racket/base

(require (for-syntax racket/base)
         congame/components/resource
         congame/components/study
         congame/components/transition-graph
         (except-in forms form)
         koyo/haml
         web-server/http)

(provide
 consent-study
 simple-study
 wrapped-simple-study)

;; Directory resources:
(define-static-resource songs "songs")

;; File resources:
(define-static-resource christmas-song (build-path "songs" "christmas.ogg"))

;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Step handlers are pure-ish.
;; Steps manage optional transitions, the default being `(next)'.

(define (info)
  (page
   `(div
     (h1 "Welcome to the study.")
     ,(attachment
       "Download CSV"
       #:filename "example.csv"
       #:content-type "application/csv"
       (lambda (out)
         (fprintf out "name,age~n")
         (fprintf out "Bogdan,31~n")))
     ,(button/confirm
       "Continue with confirmation"
       (button/confirm
        "Layer 2"
        (button/confirm
         "Layer 3")))
     ,(button void "Continue"))))

;; Steps wrap an id, a handler and a transition function.  The handler
;; must always produce an xexpression.  Actions within a handler are
;; powered by widgets, which themselves produce xexprs, but that have
;; arbitrary code which runs following user interaction.
(define (give-consent)
  (page
   (haml
    (:div
     (:h1 "Hi " (get 'name) "! Do you consent?")
     (button
      (lambda ()
        (put 'consented? #t))
      "Yes")
     (button
      (lambda ()
        (put 'consented? #f))
      "No")))))

(define name-form
  (form* ([name (ensure binding/text (required))])
    name))

(define (valid-week?) #t)
(define (week-has-passed?) #f)

(define ((enforce-week step))
  (cond
    [(valid-week?) (step)]
    [(week-has-passed?) (skip 'simple)]
    [else '(h1 "Come back later")]))

(define (tell-name)
  (page
   (haml
    (:div
     (:h1 "Tell us your name!")
     (form
      name-form
      (lambda (name)
        (put 'name name))
      (lambda (rw)
        (haml
         (:div
          (rw "name" (widget-text))
          ,@(rw "name" (widget-errors))
          (:button ([:type "submit"]) "Continue")))))))))

(define (deep-info)
  (page
   (haml
    (:div
     (:h1 "You are in the deep study")
     (button
      (lambda ()
        (put 'clicked? #t))
      "Continue")))))

(define deep-study
  (make-study
   "deep-study"
   (list
    (make-step 'deep-info deep-info))))

(define (simple-info-1)
  (page
   (haml
    (:div
     (:h1 "You are in the simple study")
     (button void "Continue")))))

(define (listen-to-some-music)
  (page
   (haml
    (:div
     (:h1 "Relax and listen to some music")
     (:audio
      ([:controls ""]
       [:src (resource-uri christmas-song)]))
     (:audio
      ([:controls ""]
       [:src (resource-uri songs "christmas.ogg")]))
     (:br)
     (button void "Continue")))))

(define (simple-info-2)
  (page
   (haml
    (:div
     (:h1 "You are still in the simple study")
     (button void "Continue")))))

(define (done-step)
  (page
   (haml
    (:h1 "Yer done."))))

(define ((echo-wrapper s))
  (printf "echo-wrapper: ~.s~n" s)
  (flush-output)
  (s))

(define simple-study
  (make-study
   "simple-study"
   #:transitions
   (transition-graph
    [simple-info-1 --> listen --> simple-info-2 --> deep --> done]
    [done --> done])
   (list
    (make-step 'simple-info-1 simple-info-1)
    (make-step 'listen listen-to-some-music)
    (make-step 'simple-info-2 simple-info-2)
    (make-step/study 'deep (map-study deep-study echo-wrapper))
    (make-step 'done done-step))))

(define wrapped-simple-study
  (map-study
   simple-study
   (lambda (hdl)
     (lambda ()
       (define the-page-thunk
         (hdl))
       (page
        `(div
          (h1 "This is our wrapper!")
          ,(the-page-thunk)
          ,(button void #:to-step-id 'done "End")))))))

(define consent-study
  (make-study
   "consent-study"
   #:requires '()
   #:provides '(consented?)
   (list
    (make-step 'info info)
    (make-step 'tell-name (enforce-week tell-name))
    (make-step 'give-consent-1
               give-consent
               (lambda ()
                 (if (get 'consented?)
                     next
                     'done))
               #:view-handler (lambda (_req)
                                (response/xexpr
                                 ((current-xexpr-wrapper)
                                  (haml
                                   (.container
                                    (:h1 "Custom View Handler")
                                    (:p "Your name is " (get 'name))))))))
    (make-step/study 'simple (map-study simple-study echo-wrapper))
    (make-step 'give-consent-2 give-consent)
    (make-step 'done done-step))))
