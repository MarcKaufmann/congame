#lang racket/base

(require (for-syntax racket/base)
         congame/components/resource
         congame/components/study
         (except-in forms form)
         koyo/haml
         racket/runtime-path)

(provide
 consent-study
 simple-study)

;; Directory resources:
(define-runtime-path songs-path "songs")
(define-static-resource songs songs-path)

;; File resources:
(define-runtime-path christmas-song-path (build-path "songs" "christmas.ogg"))
(define-static-resource christmas-song christmas-song-path)

;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Step handlers are pure-ish.
;; Steps manage optional transitions, the default being `(next)'.

(define (info)
  `(div
    (h1 "Welcome to the study.")
    ,(button void "Continue")))

;; Steps wrap an id, a handler and a transition function.  The handler
;; must always produce an xexpression.  Actions within a handler are
;; powered by widgets, which themselves produce xexprs, but that have
;; arbitrary code which runs following user interaction.
(define (give-consent)
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
     "No"))))

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
         (:button ([:type "submit"]) "Continue"))))))))

(define (deep-info)
  (haml
   (:div
    (:h1 "You are in the deep study")
    (button
     (lambda ()
       (put 'clicked? #t))
     "Continue"))))

(define deep-study
  (make-study
   (list
    (make-step 'deep-info deep-info))))

(define (simple-info-1)
  (haml
   (:div
    (:h1 "You are in the simple study")
    (button void "Continue"))))

(define (listen-to-some-music)
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
    (button void "Continue"))))

(define (simple-info-2)
  (haml
   (:div
    (:h1 "You are still in the simple study")
    (button void "Continue"))))

(define ((echo-wrapper s))
  (printf "echo-wrapper: ~.s~n" s)
  (flush-output)
  (s))

(define simple-study
  (make-study
   (list
    (make-step 'simple-info-1 simple-info-1)
    (make-step 'listen listen-to-some-music)
    (make-step 'simple-info-2 simple-info-2)
    (make-step/study 'deep (wrap-sub-study deep-study echo-wrapper)))))

(define consent-study
  (make-study
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
                     done)))
    (make-step/study 'simple (wrap-sub-study simple-study echo-wrapper))
    (make-step 'give-consent-2
               give-consent))))
