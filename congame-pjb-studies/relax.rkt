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
 (prefix-in config: (only-in congame-web/config support-email debug))
 (prefix-in bot: (submod congame/components/bot actions)))

(provide
 relax-study
 audio-container)

;; Directory resource
(define-static-resource songs "songs")
(define-static-resource audio-player.js "audio-player.js")

(define/contract (audio-container song-name #:caption [caption ""])
  (->* (string?)
       (#:caption string?)
       any)
  (haml
   (:figure#audio-container
    (:audio#audio-track ([:src (resource-uri songs song-name)]))
    (:div#audio-controls
     (:button#play ((:type "button")) "Play")
     (:button#pause ((:type "button")) "Pause")
     (:button#volume-up ((:type "button")) "Vol+")
     (:button#volume-down ((:type "button")) "Vol-"))
    (:figcaption caption)
    (:script
     ([:defer "defer"]
      [:type "text/javascript"]
      [:src (resource-uri audio-player.js)])))))

(define (explain-relaxing)
  (page
   (haml
    (.container
     (:h1 "Three Songs")
     (:p "You will listen to a different song (2-4 minutes) on each of the next three pages. You can continue to the following page only after listening to the song. After having listened to the three songs, you will be asked to rank the songs.")
     (button
      void
      "Continue")))))

(define (play-songs)
  (define song-names (get 'songs-to-play))
  (define songs-played (get 'songs-played-so-far))
  (define next-song-name
    (list-ref song-names songs-played))
  (define song-display-name
    (string-append "Song " (number->string (add1 songs-played))))
  (page
   (haml
    (.container
     (:h1 "Play Song " song-display-name
          " out of " (number->string (length song-names)))
     (audio-container next-song-name #:caption song-display-name)

     (:h3 "Instructions")

     (:ul
      (:li "Press the play button to start the song.")
      (:li "The 'Continue' button will appear once the song has finished playing (songs are around 3 minutes long)."))

     (:p "If you do not see the 'Continue' button, please " (:a ((:href (string-append "mailto:" config:support-email))) "email us") ".")
     (.hide-audio-button
      (button
       (λ ()
         (put 'songs-played-so-far (add1 songs-played)))
       "Continue"))
     (when config:debug
       (haml
        (.container.debug
         (:button ((:onclick "document.querySelector('.next-button').click()"))
                  "Skip Song"))))))))

; Has to be called in a runtime context with `current-participant-id`
(define (get-song i)
  ;; FIXME: Add a special error message to get for better error handling.
  (list-ref (get 'songs-to-play) i))

(define evaluation-form
  (form* ([preferred-song (ensure binding/text (required)
                                  (one-of '(("first"  . 0)
                                            ("second" . 1)
                                            ("third"  . 2))))]
          [second-preferred-song (ensure binding/text (required)
                                  (one-of '(("first"  . 0)
                                            ("second" . 1)
                                            ("third"  . 2))))]
          [like-classical? (ensure binding/text (required)
                                   (one-of '(("yes" . yes)
                                             ("no"  . no))))]
          [heard-song-before? (ensure binding/text (required)
                                   (one-of '(("yes" . yes)
                                             ("no"  . no))))]
          )
         (hash 'preferred-song (get-song preferred-song)
               'second-preferred-song (get-song second-preferred-song)
               'like-classical? like-classical?
               'heard-song-before? heard-song-before?)))

(define (render-evaluation-form rw)
  (haml
   (:div
    (:label.radio-group "Which was your favorite song?"
                        (rw "preferred-song"
                            (widget-radio-group '(("first"  . "First song")
                                                  ("second" . "Second song")
                                                  ("third"  . "Third song")))))
    ,@(rw "preferred-song" (widget-errors))
    (:label.radio-group "Which was your second favorite song?"
                        (rw "second-preferred-song"
                            (widget-radio-group '(("first"  . "First song")
                                                  ("second" . "Second song")
                                                  ("third"  . "Third song")))))
    ,@(rw "second-preferred-song" (widget-errors))
    (:label "Do you like classical music?"
            (rw "like-classical?" (widget-radio-group '(("yes" . "Yes")
                                                        ("no"  . "No")))))
    ,@(rw "like-classical?" (widget-errors))
    (:label "Do you think that you heard any of the songs before?"
            (rw "heard-song-before?" (widget-radio-group '(("yes" . "Yes")
                                                           ("no"  . "No")))))
    ,@(rw "heard-song-before?" (widget-errors))
    (:button.button.next-button ((:type "submit")) "Submit"))))

(define (evaluate-songs)
  (page
   (haml
    (.container
     (:h1 "Song Evaluation")
     (form
      evaluation-form
      (λ (answer)
        (put 'song-survey answer))
      render-evaluation-form)
     (:h3 "20-second snippets of the songs")
     ,@(for/list ([song-name (in-list (get 'songs-to-play))]
                  [rank      (in-list '("First" "Second" "Third"))])
         (haml
          (:figure (:figcaption (string-append rank " song"))
                   (:audio ([:controls ""]
                            [:src (resource-uri songs (string-append "snip-" song-name))])))))))))

(define (evaluate-songs/bot)
  (define f (bot:find "form"))
  (define rs (bot:element-find-all f "input[type=radio]"))
  (element-click! (car rs))
  (element-click! (bot:find "button[type=submit]")))

(define (play-songs/bot)
  (void
   (element-click! (bot:find "#play"))
   (page-execute-async! (bot:current-page) "document.querySelector('.next-button').click()"))
  #;(element-click! (page-wait-for! (bot:current-page) "a.next-button")))

(define (relax-study)
  (make-study
   #:requires '()
   #:provides '()
   (list
    (make-step 'explain-relax/randomize-song-order
               explain-relaxing
               (λ ()
                 (put 'songs-to-play
                      (shuffle '("song1.mp3" "song2.mp3" "song3.mp3")))
                 (put 'songs-played-so-far 0)
                 'play-songs)
               #:for-bot bot:continuer)
    (make-step 'play-songs
               play-songs
               (λ ()
                 (if (> (length (get 'songs-to-play)) (get 'songs-played-so-far))
                     'play-songs
                     'evaluate-songs))
               #:for-bot play-songs/bot)
    (make-step 'evaluate-songs
               evaluate-songs
               #:for-bot evaluate-songs/bot))))
