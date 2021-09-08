#lang racket/base

(require
 (for-syntax racket/base)
 racket/contract
 racket/list
 koyo/haml
 marionette
 (except-in forms form)
 congame/components/formular
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
   (:figure ((:class "audio-box"))
            (:figcaption track-name)
            (:audio ([:controls ""]
                     [:src (resource-uri tracks
                                         (string-append "snip-" track-path))])))))

(define (survey-of-tracks)
  (page
   (haml
    (.container
     (:h1 "Track Survey")
     (formular
      #:bot
      ([good (#:own-track "wave-sounds")
             (#:wave-sounds 3)
             (#:guided-meditation 5)
             (#:classical-piano 5)
             (#:edm 10)])
      (haml
       (:div
        (#:own-track
         (radios
          "Below are several types of tracks for you to rate with a 20-second snippet. Which did you listen to?"
          '(("wave-sounds" . "Wave Sounds")
            ("guided-meditation" . "Guided Meditation")
            ("classical-piano" . "Classical Piano")
            ("edm" . "Electronic Dance Music (EDM)"))
          ))
        (:div.group
         (:label  "Rate each type of track below how relaxing they are on a scale from 1 to 10, where 1 means 'ennervating', 5 means 'neutral/no effect', and 10 means 'deeply relaxing'. Play the 20-second snippets below to decide.")
         (:div
          (#:wave-sounds
           (input-number "Wave Sounds" #:min 1 #:max 10)))
         (:div
          (#:guided-meditation
           (input-number "Guided Meditation" #:min 1 #:max 10)))
         (:div
          (#:classical-piano
           (input-number "Classical Piano" #:min 1 #:max 10 )))
         (:div
          (#:edm
           (input-number "Electronic Dance Music (EDM)" #:min 1 #:max 10))))
        (:div ([:class "group"])
              (:h4 "Sound Snippets")
              (figure-with-snippet-track 'wave-sounds)
              (figure-with-snippet-track 'guided-meditation)
              (figure-with-snippet-track 'classical-piano)
              (figure-with-snippet-track 'edm))
        (:button.button.next-button ((:type "submit")) "Submit")))
      (lambda (#:own-track own-track
               #:wave-sounds wave-sounds
               #:guided-meditation guided-meditation
               #:classical-piano classical-piano
               #:edm edm)
        (put 'relaxation-survey
             (hash 'own-track own-track
                   'wave-sounds-relaxation-score wave-sounds
                   'guided-meditation-relaxation-score guided-meditation
                   'classical-piano-relaxation-score classical-piano
                   'edm-relaxation-score edm))))))))

(define (survey-of-tracks/bot)
  (formular-autofill 'good))

(define (play-tracks/bot)
  (bot:show ".hide-audio-button")
  (element-click! (page-wait-for! (bot:current-page) "a.next-button")))

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
                     'survey-of-tracks))
               #:for-bot play-tracks/bot)
    (make-step 'survey-of-tracks
               survey-of-tracks
               #:for-bot survey-of-tracks/bot))))
