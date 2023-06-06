#lang racket/base

(require (prefix-in bot: (submod congame/components/bot actions))
         congame/components/formular
         congame/components/study
         (only-in forms required)
         koyo/haml
         racket/list
         racket/match)

(provide
 inline-study)

(define (info)
  (page
   (haml
    (.container
     (formular
      #:bot
      ([ok (#:emissions 42)
           (#:location "Cluj-Napoca")])
      (haml
       (:div
        "I emit "
        (#:emissions (input-number #:required? "You need to specify emissions."))
        " per year. And I live in "
        (#:location (input-text #:required? "You need to specify a location."))
        "."
        ,@(~all-errors)
        (:button ([:type "submit"]) "Submit"))))))))

(define (info/bot)
  (formular-autofill 'ok))

(define (dynamic)
  (page
   (haml
    (.container
     (formular
      #:bot
      ([ok (#:a "field a")
           (#:b "field b")])
      #:fields ([a (input-text "field a")]
                [b (input-text "field b")])
      (match (shuffle (list a b))
        [(list f1 f2)
         (haml
          (:div
           (:ul
            (:li "F1: " f1)
            (:li "F2: " f2))
           (:button ([:type "submit"]) "Submit")))]))))))

(define (dynamic/bot)
  (formular-autofill 'ok))

(define (matrix)
  (page
   (haml
    (.container
     (formular
      #:bot
      ([ok (#:computer "mac1")])
      (haml
       (:div
        (#:computer
         (make-radios
          '((mac1 . ("Apple Mac" "White"))
            (mac2 . ("Apple Mac" "Gray"))
            (dell1 . ("Dell" "Blue"))
            (dell2 . ("Dell" "Navy")))
          (lambda (options make-radio)
            (haml
             (:table
              (:thead
               (:tr
                (:th "")
                (:th "Brand")
                (:th "Color")))
              (:tbody
               ,@(for/list ([pair (in-list options)])
                   (define option (car pair))
                   (define data (cdr pair))
                   (haml
                    (:tr
                     (:td (make-radio option))
                     (:td (car data))
                     (:td (cadr data))))))))))
         {#:default "mac2"})
        (:button ([:type "submit"]) "Submit"))))))))

(define (matrix/bot)
  (formular-autofill 'ok))

(define (select-step)
  (page
   (haml
    (.container
     (formular
      #:bot
      ([ok (#:brands "apple")])
      (haml
       (:div
        (#:brands
         (select "Pick a brand:"
                 `(("apple" . "Apple")
                   ("dell"  . "Dell")))
         {#:default "dell"})
        (:button ([:type "submit"]) "Submit"))))))))

(define (select-step/bot)
  (formular-autofill 'ok))

(define (done)
  (page
   (haml
    (.container
     (:h1 "Byeee")))))

(define inline-study
  (make-study
   "inline-study"
   (list
    (make-step 'info info #:for-bot info/bot)
    (make-step 'dynamic dynamic #:for-bot dynamic/bot)
    (make-step 'matrix matrix #:for-bot matrix/bot)
    (make-step 'select select-step #:for-bot select-step/bot)
    (make-step 'done done #:for-bot bot:completer))))

(module+ main
  (require (submod congame/components/bot actions)
           congame/components/bot-maker)
  (run-bot
   #:study-url "http://127.0.0.1:5100/study/inline"
   #:username "bot@example.com"
   #:password "password"
   #:headless? #f
   #:delay 5
   ((study->bot inline-study)
    (λ (_id bot)
      (bot)))))
