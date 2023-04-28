#lang at-exp racket/base

(require congame/components/formular
         congame/components/study
         congame/components/transition-graph
         koyo/haml)

(provide pre-survey)

(define (welcome)
  (page
   (haml
    (.container
     @:h1{Welcome}

     @:p{Thank you for participating in this survey. Since this is an anonymous survey, if you close this window your progress will be lost and you will have to restart again. So make sure to only close the window once you have finished the whole survey, received the completion code, and entered it on Prolific.}

     @(formular
       (haml
        (:div
         (#:prolific-id (input-text "Please provide your prolific ID."))
         (#:country (input-text "In what country do you currently reside?"))
         submit-button)))))))


(define completion-code
  "CVH6OUIR")

(define (page-from-formular f)
  (page
   (haml
    (.container
      f))))

(define (yn-radios label)
  (map-validator
   string->symbol
   (radios label
           '(("yes" . "Yes")
             ("no"  . "No")))))

(define submit-button
  (haml (:button.button.next-button ([:type "submit"]) "Submit")))

#;(define (do-you-have-BA)
  (page-from-formular
   (formular
    (haml
     (:div
      (#:has-BA? (yn-radios "Do you have a undergraduate/bachelor degree?"))
      submit-button)))))

(define (do-you-have-BA)
    (page
     (haml
      (.container
       (:h3 "Bachelor/Undergraduate Degree")
       (formular
        (haml
         (:div
          (#:has-BA? (yn-radios "Do you have a undergraduate/bachelor degree?"))
          submit-button)))))))

(define (when-got-BA)
  (page
   (haml
    (.container
     (:h3 "Bachelor/Undergraduate Degree")
     (formular
      (haml
       (:div
        (#:when-got-BA (input-date "When did you get your undergraduate/bachelor degree?"))
        submit-button)))))))

(define (BA-in-progress)
  (page
   (haml
    (.container
     (:h3 "Bachelor/Undergraduate Degree")
     (formular
      (haml
       (:div
        (#:BA-in-progress? (yn-radios "Are you in the progress of getting a undergraduate/bachelor degree?"))
        submit-button)))))))

(define (BA-info)
  (define BA-in-progress? (equal? (get 'BA-in-progress? #f) 'yes))
  (page
   (haml
    (.container
     (:h3 "Bachelor/Undergraduate Degree")
     (formular
      (haml
       (:div
        (.group
         (:label (format "What ~a the focus of your undergraduate/bachelor? Select all that apply."
                         (if BA-in-progress? "is" "was")))
         (#:computer-science (selectbox "Computer Science"))
         (#:data-science (selectbox "Data Science/Business Analytics"))
         (#:economics (selectbox "Economics"))
         (#:engineering (selectbox "Engineering"))
         (#:humanities (selectbox "Humanities (Philosophy, History, ...)"))
         (#:natural-science (selectbox "Natural Science (Maths, Physics, ...)"))
         (#:social-science (selectbox "Social Science other than Economics (Sociology, Anthropology, ...)"))
         (#:other (selectbox "Other")))
        (.group
         (#:BA-location (input-text (format "Where ~a you studying for your undergraduate/bachelor? Select all where you spent one or more semesters." (if BA-in-progress? "are" "were")))))
        submit-button)))))))

(define (study-abroad)
  (page
   (haml
    (.container
     (:h3 "Study Abroad")
     (formular
      (haml
       (:div
        (#:study-abroad? (yn-radios "Have you considered pursuing education in a different country?"))
        submit-button)))))))

(define (places-study-abroad)
  (page
   (haml
    (.container
     (:h3 "Study Abroad")
     (formular
      (haml
       (:div
        (:h3 "Which of the following places would you consider for studying abroad? Select all that apply.")
        (#:consider-africa (selectbox "Africa"))
        (#:consider-asia (selectbox "Asia"))
        (#:consider-australia (selectbox "Australia"))
        (#:consider-central-eastern-europe (selectbox "Central and Eastern Europe"))
        (#:consider-north-america (selectbox "North America"))
        (#:consider-south-america (selectbox "South America"))
        (#:consider-western-europe (selectbox "Western Europe"))
        submit-button)))))))

(define (where-in-western-europe)
  (page
   (haml
    (.container
     (:h3 "Study Abroad")
     (formular
      (haml
       (:div
        (.group
         (:h3 "Which of the following regions in Western Europe would you consider for studying abroad? Select all that apply.")
         (#:consider-scandinavia (selectbox "Denmark, Finland, Norway, Sweden"))
         (#:consider-english (selectbox "Ireland, United Kingdom"))
         (#:consider-german (selectbox "Austria, Germany, Switzerland"))
         (#:consider-italy (selectbox "France"))
         (#:consider-france (selectbox "Italy"))
         (#:consider-iberia (selectbox "Portugal, Spain"))
         (#:consider-benelux (selectbox "Belgium, Netherlands, Luxembourg")))
        submit-button)))))))

(define (attention-check)
  (page
   (haml
    (.container
     (:p "The question regarding about cities you are about to be asked is simple: when asked for your favorite city to visit, you must type 'Lagos'. This is an attention check.")
     (formular
      (haml
       (:div
        (:h3 "Your favorite city")
        (#:favorite-city (input-text "Based on the instructions above, what is your favorite city to visit?"))
        submit-button))
      (lambda (#:favorite-city favorite-city)
        (put 'favorite-city favorite-city)
        (put 'pass-attention-check? (string=? (string-downcase favorite-city) "lagos"))))))))

(define (thank-you)
  (page
   (haml
    (.container
     @:h1{Thank you!}

     @:p{Thank you for participating in this survey. The completion code is @(:strong completion-code), please submit it on prolific to complete this study.}

     @:p{The prolific ID that you provided is @(:strong (get 'prolific-id "#<error: could not find your ID, please contact us")). If this is not correct, please contact us.}))))

(define pre-survey
  (make-study
   "pre-survey"
   #:transitions
   (transition-graph
    (welcome --> do-you-have-BA
             --> ,(lambda ()
                    (case (get 'has-BA?)
                      [(yes) (goto when-got-BA)]
                      [(no)  (goto BA-in-progress)])))

    (when-got-BA --> BA-info)

    (BA-in-progress --> ,(lambda ()
                           (case (get 'BA-in-progress?)
                             [(yes) (goto BA-info)]
                             [(no)  (goto study-abroad)])))

    (BA-info --> study-abroad
             --> ,(lambda ()
                    (case (get 'study-abroad?)
                      [(yes) (goto places-study-abroad)]
                      [(no)  (goto attention-check)])))

    (places-study-abroad --> ,(lambda ()
                                (if (get 'consider-western-europe)
                                  (goto where-in-western-europe)
                                  (goto attention-check))))

    (where-in-western-europe --> attention-check --> thank-you)

    (thank-you --> thank-you))

   (list
    (make-step 'welcome welcome)
    (make-step 'do-you-have-BA do-you-have-BA)
    (make-step 'when-got-BA when-got-BA)
    (make-step 'BA-info BA-info)
    (make-step 'BA-in-progress BA-in-progress)
    (make-step 'study-abroad study-abroad)
    (make-step 'places-study-abroad places-study-abroad)
    (make-step 'where-in-western-europe where-in-western-europe)
    (make-step 'attention-check attention-check)
    (make-step 'thank-you thank-you))))
