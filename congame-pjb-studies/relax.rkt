#lang racket

(require
 koyo/haml
 congame/components/resource
 congame/components/study
 congame/components/template
 (prefix-in bot: (submod congame/components/bot actions)))

(provide
 relax-study
 audio-container)

;; File resources:
(define-static-resource song1 (build-path "songs" "song1.mp3"))
(define-static-resource song2 (build-path "songs" "song2.mp3"))
(define-static-resource song3 (build-path "songs" "song3.mp3"))

(define songs
  (hash 'song1 song1
        'song2 song2
        'song3 song3))

(define (audio-container resource #:caption [caption ""])
  (haml
   (:figure#audio-container
    ;; FIXME: Why does `(resource-uri song1)` explode with `application: not a procedure;` error?
    ;; Some compile-time vs runtime thingy-thingy?
    (:audio#audio-track ([:src (resource-uri resource)]))
    (:div#audio-controls
     (:button#play ((:type "button")) "Play")
     (:button#pause ((:type "button")) "Pause")
     (:button#volume-up ((:type "button")) "Vol+")
     (:button#volume-down ((:type "button")) "Vol-"))
    (:figcaption "What a song"))))

(define (explain-relaxing)
  ((page/xexpr)
   (haml
    (.container
     (:h1 "Three Songs")
     (:p "You will listen to a different song on each of the next three pages. Each song is between 3 and 4 minutes long. You can continue to the following page only after having listened until the end of the song. After that, you will be asked to rank the songs according along several dimensions.")
     (button
      void
      "Continue")))))

(define (play-songs)
  (define song-names (get 'songs-to-play))
  (define songs-played (get 'songs-played-so-far))
  (define next-song-name
    (list-ref song-names songs-played))
  (define next-song
    (hash-ref songs next-song-name))
  (displayln next-song)
  (flush-output)
  ((page/xexpr)
   (haml
    (.container
     (:h1 "Play a Song")
     (audio-container next-song)
     (.hide-audio-button
      (button
       (λ ()
         (put 'songs-played-so-far (add1 songs-played)))
       "Continue"))))))

(define (evaluate-songs)
  ((page/xexpr)
   (haml
    (.container
     (:h1 "Evaluate Songs")
     (button void "Continue")))))



;; FIXME: It would be better if randomization could also be done at the study rather than step level.
;; That's more natural. If we can write transitions study-wide, than this is perfectly doable.
(define (relax-study)
  (make-study
   #:requires '()
   #:provides '()
   (list
    (make-step 'explain-relax/randomize-song-order
               explain-relaxing
               (λ ()
                 (put 'songs-to-play
                      (shuffle '(song1 song2 song3)))
                 (put 'songs-played-so-far 0)
                 'play-songs)
               #:for-bot bot:continuer)
    (make-step 'play-songs
               play-songs
               (λ ()
                 (if (> (length (get 'songs-to-play)) (get 'songs-played-so-far))
                     'play-songs
                     'evaluate-songs))
               #:for-bot bot:continuer)
    (make-step 'evaluate-songs evaluate-songs))))
