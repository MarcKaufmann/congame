#lang racket/base

(require congame/components/formular
         congame/components/study
         congame/components/transition-graph
         koyo/haml
         racket/format)

(provide src-survey)

(define (route)
  (skip))

(define submit-button
  (haml
   (:button.button.next-button ([:type "submit"]) "Submit")))

(define (start)
  (page
   (haml
    (.container
     (:h1 "Start the survey")

     (button void "Start Survey")))))

; "consumption/production"
; "total consumption vs local consumption: at this shop, in Vienna, in Austria, in Europe"
; "going up for buying/vs going down for not buying"

(define (input-dampening product)
  (input-number
   (format "By how many units does total demand of ~a go up if one consumer buys one extra ~a?" product product)
   #:step 0.01))

(define (product-dampening)
  (page
   (haml
    (.container
     (:h3 "Consumption Choices")

     (:p "Consider a single consumer who is choosing between buying exactly one more unit of some good or not buying it. The following questions ask you to state how much buying, as opposed to not buying, the extra unit increases " (:strong "total") " demand of this good.")

     (formular
      (haml
       (:div
        (#:coffee-impact (input-dampening "coffee"))
        (#:flights-impact (input-dampening "flights"))
        (#:steak-impact (input-dampening "steak"))
        submit-button))
      (make-put-form/cons/instance))))))

(define (product-dampening-after)
  (page
   (haml
    (.container
     (:h3 "Consumption Choices")

     (:p "Consider a single consumer who is choosing between buying exactly one more unit of some good or not buying it. The following questions ask you to state how much buying, as opposed to not buying, the extra unit increases " (:strong "total") " demand of this good.")

     (formular
      (haml
       (:div
        (#:coffee-impact-after (input-dampening "coffee"))
        (#:flights-impact-after (input-dampening "flights"))
        (#:steak-impact-after (input-dampening "steak"))
        submit-button))
      (make-put-form/cons/instance))))))

(define (input-likert label)
  (input-number label #:min 1 #:max 7))

(define (src-contribution)
  (page
   (haml
    (.container
     (:h3 "Socially Responsible Consumption")

     (:p "Some consumers are willing to change their consumption choices in order to support or achieve some larger social goal. The following questions ask how much you believe that consumption choices can impact these final outcomes.")
     (formular
      (haml
       (:div
        (#:src-contribution (input-likert "What do you think is the potential impact of socially responsible consumption --- and thus of encouraging more socially responsible consumption --- towards achieving social goals on a scale from 1 to 7, where 1 means 'cannot contribute' and 7 means 'can fully solve'?"))
        submit-button))
      (make-put-form/cons/instance))))))

(define (src-contribution-after)
  (page
   (haml
    (.container
     (:h3 "Socially Responsible Consumption")

     (:p "Some consumers are willing to change their consumption choices in order to support or achieve some larger social goal. The following questions ask how much you believe that consumption choices can impact these final outcomes.")
     (formular
      (haml
       (:div
        (#:src-contribution-after (input-likert "What do you think is the potential impact of socially responsible consumption --- and thus of encouraging more socially responsible consumption --- towards achieving social goals on a scale from 1 to 7, where 1 means 'cannot contribute' and 7 means 'can fully solve'?"))
        submit-button))
      (make-put-form/cons/instance))))))

(define (src-contribution-topics)
  (page
   (haml
    (.container
     (:h3 "Socially Responsible Consumption")

     (:p "Now consider the potential of socially responsible consumption to contribute towards specific goals. For each of the following social goals, state how much you think socially responsible consumption can contribute towards achieving this particular goal on a scale from 1 to 7, where 1 means 'cannot contribute' and 7 means 'can fully solve'.")

     (formular
      (haml
       (:div
        (#:src-contribution-climate-change (input-likert "How much can socially responsible consumption contribute towards solving climate change?"))
        (#:src-contribution-human-rights (input-likert "How much can socially responsible consumption contribute towards strengthening human rights?"))
        (#:src-contribution-animal-rights (input-likert "How much can socially responsible consumption contribute towards avoiding animal suffering?"))
        submit-button))
      (make-put-form/cons/instance))))))

(define ((waiting-page-for phase))
  (when (equal? (get/instance 'phase 'none) phase)
    (skip))
  (page
   (haml
    (.container
     (:h1 "Please wait for the next phase to start")))))

(define (phase-button phase)
  (button (lambda ()
            (put/instance 'phase phase))
          (format "Move to ~a Phase" phase)))

(define (admin)
  (page
   (haml
    (.container
     (:h1 "Admin")

     (phase-button 'survey)
     (phase-button 'IDM)
     (phase-button 'treatments)
     (phase-button 'end)

     (:h3 "Results")

     (button void "SRC before" #:to-step-id 'admin-src-before)
     (button void "SRC after" #:to-step-id 'admin-src-after)
     (button void "Prediction" #:to-step-id 'admin-predictions)))))

(define ids-to-labels
    (hash 'steak-impact                    "Steaks"
          'flights-impact                  "Flights"
          'coffee-impact                   "coffee"
          'src-contribution                "Socially Responsible Consumption"
          'src-contribution-animal-rights  "SRC Animal Rights"
          'src-contribution-human-rights   "SRC Human Rights"
          'src-contribution-climate-change "SRC Climate Change"
          'steak-impact-after              "Steaks (after)"
          'flights-impact-after            "Flights (after)"
          'coffee-impact-after             "Coffee (after)"
          'src-contribution-after          "Socially Responsible Consumption (after)"
          'prediction-IDM                  "Prediction for IDM"
          'prediction-single               "Prediction for SINGLE"
          'prediction-multi                "Prediction for MULTI"
          'prediction-full                 "Prediction for FULL"))

(define ((admin-show-averages loq))
  (define (mean xs)
    (/ (apply + xs) (* 1.0 (length xs))))

  (page
   (haml
    (.container
     (:h1 "Results")

     (:ul
      ,@(for/list ([q (in-list loq)])
          (define answers
            (filter number? (get/instance q null)))
          (haml
           (:li (hash-ref ids-to-labels q (format "#<Error: Label not found for ~a>" q))
                ": "
                (if (> (length answers) 0)
                    (~r (mean answers) #:precision 2)
                    "(no answers yet)")))))

     (button void "Back to Admin")))))

(define (prediction-IDM)
  (page
   (haml
    (.container
     (:h1 "Prediction of Ziegler et al (2022)")

     (formular
      (haml
       (:div
        (#:prediction-IDM (input-number "Guess the mean amount of money (in Euros) that participants were willing to give up for a donation in the individual decision-making task. That is, the highest amount they were willing to give up in order to enable one more donation." #:min 0 #:max 5 #:step 0.1))
        submit-button))
      (make-put-form/cons/instance))))))

(define (prediction-treatments)
  (page
   (haml
    (.container
     (:h1 "Prediction of Ziegler et al (2022)")

     (formular
      (haml
       (:div
        (:p "Consider the treatments.")
        (:p "In SINGLE, each of the 5 buyers could buy at most 1 unit and each of the 5 sellers could sell at most 1 unit, so at most 5 units can be traded.")
        (:p "In MULTI, each of the 5 buyers could buy at most 3 units and each of the 5 sellers could sell at most 3 units, for a total of at most 15 units traded.")
        (:p "In FULL, at most 15 units can be traded, and each buyer could buy as many units as they want, and each seller could buy as many as they wanted, so long as less than 15 units have been traded so far.")
        (#:prediction-single (input-number "What is the percent of units traded in SINGLE? If you think 1 unit was traded, you should answer 20%, 2 units is 40%, and so on." #:min 0 #:max 100))
        (#:prediction-multi (input-number "What is the percent of units traded in MULTI? If you think 3 units were traded, you should answer 20%, 6 units is 40%, and so on." #:min 0 #:max 100))
        (#:prediction-full (input-number "What is the percent of units traded in FULL? If you think 3 units were traded, you should answer 20%, 6 units is 40%, and so on." #:min 0 #:max 100))
        submit-button))
      (make-put-form/cons/instance))))))

(define (end)
  (page
   (haml
    (.container
     (:h1 "End")
     (:p "Thank you for participating in this survey")))))

(define src-survey
  (make-study
   "src-survey"

   #:transitions
   (transition-graph
    (route --> ,(lambda ()
                  (if (current-participant-owner?)
                      (goto admin)
                      (goto start))))
    (admin --> admin)
    (admin-src-before --> admin)
    (admin-src-after --> admin)
    (admin-predictions --> admin)

    (start --> wait-for-survey
           --> product-dampening
           --> src-contribution
           --> src-contribution-topics
           --> wait-for-IDM
           --> prediction-IDM
           --> wait-for-treatments
           --> prediction-treatments
           --> wait-for-end
           --> product-dampening-after
           --> src-contribution-after
           --> end
           --> end))

   (list
    (make-step 'route route)
    (make-step 'admin admin)
    (make-step 'admin-src-before
               (admin-show-averages
                (list 'coffee-impact 'flights-impact 'steak-impact 'src-contribution 'src-contribution-animal-rights 'src-contribution-human-rights 'src-contribution-climate-change)))
    (make-step 'admin-predictions
               (admin-show-averages
                (list 'prediction-IDM 'prediction-single 'prediction-multi 'prediction-full)))
    (make-step 'admin-src-after
               (admin-show-averages
                (list 'coffee-impact-after 'flights-impact-after 'steak-impact-after 'src-contribution-after)))
    (make-step 'start start)
    (make-step 'wait-for-survey (waiting-page-for 'survey))
    (make-step 'product-dampening product-dampening)
    (make-step 'src-contribution src-contribution)
    (make-step 'src-contribution-topics src-contribution-topics)
    (make-step 'wait-for-IDM (waiting-page-for 'IDM))
    (make-step 'prediction-IDM prediction-IDM)
    (make-step 'wait-for-treatments (waiting-page-for 'treatments))
    (make-step 'prediction-treatments prediction-treatments)
    (make-step 'wait-for-end (waiting-page-for 'end))
    (make-step 'product-dampening-after product-dampening-after)
    (make-step 'src-contribution-after src-contribution-after)
    (make-step 'end end))))
