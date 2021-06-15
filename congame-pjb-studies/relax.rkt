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
     (:h1 (format "~a Audio/Meditation Track~a (5-7 minutes total)"
                  n
                  (if (> n 1) "s" "")))
     (:p (format "You will listen to a different audio track on ~a, followed by a question page. On each page, the continue button appears once the track has finished playing."
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
      (:li "The 'Continue' button will appear once the track has finished playing (all tracks together are around 5-7 minutes long)."))

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

(define ((lift f) v)
  (if v
      (f v)
      (ok v)))

(define (inclusive-between low high #:message [message #f])
  (lift (lambda (v)
          (if (<= low v high)
              (ok v)
              (err (or message (format "value ~a is not between low ~a and high ~a" v low high)))))))

(define evaluation-form
  (form* ([own-track (ensure binding/text (required)
                             (one-of '(("wave-sounds" 'wave-sounds)
                                       ("guided-meditation" 'guided-meditation)
                                       ("classical-piano" 'classical-piano)
                                       ("edm" 'edm))))]
          [wave-sounds-relaxation-score (ensure binding/number (required) (inclusive-between 1 10))]
          [guided-meditation-relaxation-score (ensure binding/number (required) (inclusive-between 1 10))]
          [classical-piano-relaxation-score (ensure binding/number (required) (inclusive-between 1 10))]
          [edm-relaxation-score (ensure binding/number (required) (inclusive-between 1 10))])
         (hash 'own-track own-track
               'wave-sounds-relaxation-score wave-sounds-relaxation-score
               'guided-meditation-relaxation-score guided-meditation-relaxation-score
               'classical-piano-relaxation-score classical-piano-relaxation-score
               'edm-relaxation-score edm-relaxation-score)))

(define (widget-10-scale)
  (widget-number #:attributes '((min "1") (max "10"))))

(define (figure-with-snippet-track track-treatment)
  (define track-path
    (car (hash-ref (get 'tracks-to-evaluate) track-treatment)))
  (define track-name
    (case track-treatment
      [(classical-piano) "Classical Piano"]
      [(guided-meditation) "Guided Meditation"]
      [(wave-sounds) "Wave Sounds"]
      [(edm) "Electronic Dance Music (EDM)"]))
  (haml
   (:figure (:figcaption track-name)
            (:audio ([:controls ""]
                     [:src (resource-uri tracks
                                         (string-append "snip-" track-path))])))))

(define (render-evaluation-form rw)
  (haml
   (:div
    (:div
     (:label.radio-group "Below are several types of tracks for you to rate with a 20-second snippet. Which did you listen to?"
                         (rw "own-track" (widget-radio-group '(("wave-sounds" . "Wave Sounds")
                                                               ("guided-meditation" . "Guided Meditation")
                                                               ("classical-piano" . "Classical Piano")
                                                               ("edm" . "Electronic Dance Music (EDM)")))))
     ,@(rw "own-track" (widget-errors)))
    (:div ([:class "group"])
          (:label "Rate each type of track below On a scale from 1 to 10, where 1 means 'ennervating', 5 means 'neutral/no effect', and 10 means 'deeply relaxing'. You can play a 20-second snippet below.")
          (:div
           (:label "Wave Sounds" (rw "wave-sounds-relaxation-score" (widget-10-scale)))
           ,@(rw "wave-sounds-relaxation-score" (widget-errors)))
          (:div
           (:label "Guided Meditation" (rw "guided-meditation-relaxation-score" (widget-10-scale)))
           ,@(rw "guided-meditation-relaxation-score" (widget-errors)))
          (:div
           (:label "Classical Piano" (rw "classical-piano-relaxation-score" (widget-10-scale)))
           ,@(rw "classical-piano-relaxation-score" (widget-errors)))
          (:div
           (:label "Electronic Dance Music (EDM)" (rw "edm-relaxation-score" (widget-10-scale)))
           ,@(rw "edm-relaxation-score" (widget-errors))))
    (:div ([:class "group"])
     (:h4 "Sound Snippets")
     (figure-with-snippet-track 'wave-sounds)
     (figure-with-snippet-track 'guided-meditation)
     (figure-with-snippet-track 'classical-piano)
     (figure-with-snippet-track 'edm))
    (:button.button.next-button ((:type "submit")) "Submit"))))

(define (track-survey)
  (page
   (haml
    (.container
     (:h1 "Track Survey")
     (form
      evaluation-form
      (λ (answer)
        (put 'track-survey answer))
      render-evaluation-form)))))

(define (track-survey/bot)
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
   #:requires '(tracks-to-play tracks-to-evaluate)
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
                     'track-survey))
               #:for-bot play-tracks/bot)
    (make-step 'track-survey
               track-survey
               #:for-bot track-survey/bot))))
