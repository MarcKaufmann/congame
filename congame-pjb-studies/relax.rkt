#lang racket/base

(require
 (for-syntax racket/base)
 racket/contract
 racket/list
 koyo/haml
 marionette
 (except-in forms form)
 congame/components/resource
 congame/components/study
 (prefix-in config: congame/config)
 (prefix-in bot: (submod congame/components/bot actions)))

(provide
 relax-study
 audio-container)

;; Directory resource
(define-static-resource tracks "tracks")
(define-static-resource audio-player.js "audio-player.js")

(define/contract (audio-container track-name #:caption [caption ""])
  (->* (string?)
       (#:caption string?)
       any)
  (haml
   (:figure#audio-container
    (:audio#audio-track ([:src (resource-uri tracks track-name)]))
    (:div#audio-controls
     (:button#play ((:type "button")) "Play")
     (:button#pause ((:type "button")) "Pause")
     (:button#volume-up ((:type "button")) "Vol+")
     (:button#volume-down ((:type "button")) "Vol-"))
    (:figcaption caption)
    (:script
     ([:type "text/javascript"]
      [:src (resource-uri audio-player.js)])))))

(define (explain-relaxing)
  (define n (length (get 'tracks-to-play)))
  (page
   (haml
    (.container
     (:h1 (format "~a Audio Track~a (5-7 minutes total)"
                  n
                  (if (> n 1) "s" "")))
     (:p (format "You will listen to a different audio track on ~a, followed by a question page. The continue button appears once the track has finished playing."
                 (if (> n 1)
                     (string-append "each of the next " (number->string n) " pages")
                     "the next page")))
     (button

      void
      "Continue")))))

(define (play-tracks)
  (define track-names (get 'tracks-to-play))
  (define tracks-played (get 'tracks-played-so-far))
  (define next-track-name
    (list-ref track-names tracks-played))
  (define track-display-name
    (string-append "Track " (number->string (add1 tracks-played))))
  (page
   (haml
    (.container
     (:h1 "Play " track-display-name
          " out of " (number->string (length track-names)))
     (audio-container next-track-name #:caption track-display-name)

     (:h3 "Instructions")

     (:ul
      (:li "Press the play button to start the track")
      (:li "Sit back and listen to the track")
      (:li "The 'Continue' button will appear once the track has finished playing (tracks are around 3 minutes long)."))

     (:p "If you do not see the 'Continue' button, please " (:a ((:href (string-append "mailto:" config:support-email))) "email us") ".")
     (.hide-audio-button
      (button
       (λ ()
         (put 'tracks-played-so-far (add1 tracks-played)))
       "Continue"))
     (when config:debug
       (haml
        (.container.debug
         (:button ((:onclick "document.querySelector('.next-button').click()"))
                  "Skip Track"))))))))

; Has to be called in a runtime context with `current-participant-id`
(define (get-track i)
  ;; FIXME: Add a special error message to get for better error handling.
  (list-ref (get 'tracks-to-play) i))

(define evaluation-form
  (form* ([preferred-track (ensure binding/text (required)
                                  (one-of '(("first"  . 0)
                                            ("second" . 1)
                                            ("third"  . 2))))]
          [second-preferred-track (ensure binding/text (required)
                                  (one-of '(("first"  . 0)
                                            ("second" . 1)
                                            ("third"  . 2))))]
          [like-classical? (ensure binding/text (required)
                                   (one-of '(("yes" . yes)
                                             ("no"  . no))))]
          [heard-track-before? (ensure binding/text (required)
                                   (one-of '(("yes" . yes)
                                             ("no"  . no))))]
          )
         (hash 'preferred-track (get-track preferred-track)
               'second-preferred-track (get-track second-preferred-track)
               'like-classical? like-classical?
               'heard-track-before? heard-track-before?)))

(define (render-evaluation-form rw)
  (haml
   (:div
    (:label.radio-group "Which was your favorite track?"
                        (rw "preferred-track"
                            (widget-radio-group '(("first"  . "First track")
                                                  ("second" . "Second track")
                                                  ("third"  . "Third track")))))
    ,@(rw "preferred-track" (widget-errors))
    (:label.radio-group "Which was your second favorite track?"
                        (rw "second-preferred-track"
                            (widget-radio-group '(("first"  . "First track")
                                                  ("second" . "Second track")
                                                  ("third"  . "Third track")))))
    ,@(rw "second-preferred-track" (widget-errors))
    (:label.radio-group "Do you like classical music?"
                        (rw "like-classical?" (widget-radio-group '(("yes" . "Yes")
                                                                    ("no"  . "No")))))
    ,@(rw "like-classical?" (widget-errors))
    (:label.radio-group "Do you think that you heard any of the tracks before?"
                        (rw "heard-track-before?" (widget-radio-group '(("yes" . "Yes")
                                                                       ("no"  . "No")))))
    ,@(rw "heard-track-before?" (widget-errors))
    (:button.button.next-button ((:type "submit")) "Submit"))))

(define (evaluate-tracks)
  (page
   (haml
    (.container
     (:h1 "Track Evaluation")
     (form
      evaluation-form
      (λ (answer)
        (put 'track-survey answer))
      render-evaluation-form)
     (:h3 "20-second snippets of the tracks")
     ,@(for/list ([track-name (in-list (get 'tracks-to-play))]
                  [rank      (in-list '("First" "Second" "Third" "Fourth" "Fifth"))])
         (haml
          (:figure (:figcaption (string-append rank " track"))
                   (:audio ([:controls ""]
                            [:src (resource-uri tracks (string-append "snip-" track-name))])))))))))

(define (evaluate-tracks/bot)
  (define f (bot:wait-for "form"))
  (for ([group-el (bot:element-find-all f ".radio-group")])
    (define first-radio-el (bot:element-find group-el "input[type=radio]"))
    (element-click! first-radio-el))
  (element-click! (bot:find "button[type=submit]")))

(define (play-tracks/bot)
  (void
   (element-click! (bot:find "#play"))
   (page-execute-async! (bot:current-page) "document.querySelector('.next-button').click()"))
  #;(element-click! (page-wait-for! (bot:current-page) "a.next-button")))

(define (relax-study)
  (make-study
   "relax-study"
   #:requires '(tracks-to-play)
   #:provides '()
   (list
    (make-step 'explain-relax
               explain-relaxing
               (λ ()
                 (put 'tracks-played-so-far 0)
                 'play-tracks)
               #:for-bot bot:continuer)
    (make-step 'play-tracks
               play-tracks
               (λ ()
                 (if (> (length (get 'tracks-to-play)) (get 'tracks-played-so-far))
                     'play-tracks
                     'evaluate-tracks))
               #:for-bot play-tracks/bot)
    (make-step 'evaluate-tracks
               evaluate-tracks
               #:for-bot evaluate-tracks/bot))))
