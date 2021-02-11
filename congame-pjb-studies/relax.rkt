#lang racket/base

(require
 (for-syntax racket/base)
 racket/contract
 racket/list
 racket/random
 koyo/haml
 marionette
 (except-in forms form)
 congame/components/resource
 congame/components/study
 (prefix-in config: (only-in congame-web/config support-email))
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
  (haml
   (.container
    (:h1 "Three Songs")
    (:p "You will listen to a different song on each of the next three pages. Each song is between 3 and 4 minutes long. You can continue to the following page only after having listened until the end of the song. After that, you will be asked to rank the songs according along several dimensions.")
    (button
     void
     "Continue"))))

(define (play-songs)
  (define song-names (get 'songs-to-play))
  (define songs-played (get 'songs-played-so-far))
  (define next-song-name
    (list-ref song-names songs-played))
  (haml
   (.container
    (:h1 "Play Song " (number->string (add1 songs-played))
         " out of " (number->string (length song-names)))
    (audio-container next-song-name #:caption "What a song...")
    (.hide-audio-button
     (button
      (λ ()
        (put 'songs-played-so-far (add1 songs-played)))
      "Continue"))
    (:h3 "Instructions")

    (:ul
     (:li "Press the play button to start the song.")
     (:li "The 'Continue' button will appear once the song has finished playing."))

    (:p "If you do not see the 'Continue' button, please " (:a ((:href (string-append "mailto:" config:support-email))) "email us") "."))))

; Has to be called in a runtime context with `current-participant-id`
(define (get-song i)
  ;; FIXME: Add a special error message to get for better error handling.
  (list-ref (get 'songs-to-play) i))

(define evaluation-form
  (form* ([preferred-song (ensure binding/text (required)
                                  (one-of '(("first"  . 0)
                                            ("second" . 1)
                                            ("third"  . 2))))])
         (get-song preferred-song)))

(define (render-evaluation-form rw)
  (haml
   (:div
    (:label.radio-group "Which was your favorite song?"
                        (rw "preferred-song"
                            (widget-radio-group '(("first"  . "First song")
                                                  ("second" . "Second song")
                                                  ("third"  . "Third song")))))
    ,@(rw "preferred-song" (widget-errors))
    (:button.button.next-button ((:type "submit")) "Submit"))))

(define (evaluate-songs)
  (haml
   (.container
    (:h1 "Song Evaluation")
    (form
     evaluation-form
     (λ (answer)
       (displayln (format "Favorite song is ~a" answer))
       (flush-output))
     render-evaluation-form)
    (:h3 "Less than 5-second snippets of the songs")
    ,@(for/list ([song-name (in-list (get 'songs-to-play))]
                 [rank      (in-list '("First" "Second" "Third"))])
        (haml
         (:figure (:figcaption (string-append rank " song"))
                  (:audio ([:controls ""]
                           [:src (resource-uri songs (string-append "snip-" song-name))]))))))))

(define (evaluate-songs/bot)
  (define f (bot:find "form"))
  (define rs (bot:element-find-all f "input[type=radio]"))
  (element-click! (car rs))
  (element-click! (bot:find "button[type=submit]")))

(define (play-songs/bot)
  (element-click! (bot:find "#play"))
  #;(void
   (page-execute-async! (bot:current-page) "document.querySelector('form').submit()"))
  (element-click! (page-wait-for! (bot:current-page) "a.next-button")))

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
