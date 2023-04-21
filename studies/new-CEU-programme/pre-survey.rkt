#lang at-exp racket/base

(require congame/components/formular
         congame/components/study
         congame/components/transition-graph
         koyo/haml)

(provide pre-survey)

(define (stub)
  (page
   (haml
    (.container
     @:h1{Stub}
     @button[void]{Next}))))

(define (welcome)
  (page
   (haml
    (.container
     @:h1{Welcome}

     @:p{Thank you for participating in this survey.}

     @button[void]{Start Survey}))))

(define completion-code
  "BLA")

(define (do-you-have-BA)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:has-BA? (radios "Do you have a bachelor or undergraduate degree?"
                           '(("yes" . "Yes")
                             ("no"  . "No"))))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:has-BA? has-BA?)
        (put 'has-BA? (string->symbol has-BA?))))))))

(define (thank-you)
  (page
   (haml
    (.container
     @:h1{Thank you!}

     @:p{Thank you for participating in this survey. The completion code is @completion-code, please submit it on prolific to complete this study.} ))))

(define pre-survey
  (make-study
   "pre-survey"
   #:transitions
   (transition-graph
    (welcome --> do-you-have-BA
             --> ,(lambda ()
                    (case (get 'has-BA?)
                      [(yes) (goto when-got-BA)]
                      [(no)  (goto no-BA)])))

    (when-got-BA --> focus-of-BA)

    (no-BA --> BA-in-progress
           --> ,(lambda ()
                  (case (get 'BA-in-progress?)
                    [(yes) (goto focus-of-BA)]
                    [(no)  (goto study-abroad)])))

    (focus-of-BA --> study-abroad)

    (study-abroad --> ,(lambda ()
                            (case (get 'study-abroad?)
                              [(yes) (goto places-study-abroad)]
                              [(no)  (goto thank-you)])))

    (places-study-abroad --> thank-you)

    (thank-you --> thank-you))

   (list
    (make-step 'welcome welcome)
    (make-step 'do-you-have-BA do-you-have-BA)
    (make-step 'when-got-BA stub)
    (make-step 'no-BA stub)
    (make-step 'focus-of-BA stub)
    (make-step 'BA-in-progress stub)
    (make-step 'study-abroad stub)
    (make-step 'places-study-abroad stub)
    (make-step 'thank-you thank-you))))
