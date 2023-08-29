#lang racket/base

(require racket/random
         congame/components/formular
         congame/components/study
         congame/components/transition-graph
         (submod congame/components/formular tools)
         koyo/haml)

(provide
 edpb-reasons-pilot)

(define payment 1.5)
(define completion-code "ABCDEFG")

(define (welcome-and-consent)
  (page
   (haml
    (.container
     (:h1 "Welcome")

     (:p (format "The following short survey asks your opinion about, and familiarity with, specific topics. It takes around ten minutes to complete, and you will receive ~a GBP as a completion bonus. Please answer the questions truthfully, as they provide valuable information to us for another study. We will not reveal your individual answers to the participants of the subsequent study. Instead, we will show them statistics based on the responses of all participants in this survey. We will not be able to link your answers and your identity beyond the Prolific ID."
                 payment))

     (:p "If you agree to participate in the survey, you can still opt out anytime. However, you only receive the completion bonus if you complete the whole survey.")

     (formular
      (haml
       (:div
        (#:consent?
         (radios "Do you agree to participate in the survey?"
                 '(("yes" . "Yes")
                   ("no"  . "No"))))
        submit-button)))))))

(define (no-consent)
  (page
   (haml
    (.container
     (:h1 "Thank you")

     (:p "You did not agree to participate in the survey. Thank you for taking the time to consider it.")))))

(define (input-likert label)
  (input-number label #:min 1 #:max 7))

(define ((topic-survey topic))
  (page
   (haml
    (.container
     (:h3 "Topic: ~a" topic)

     (:p (format "The next topic is ~a. Please answer the following questions for this topic." topic))

     (formular
      (haml
       (:div
        (:div
         (#:familiar-A
          (input-likert "How familiar are you with the topic on a scale from 1 (never heard about it) to 7 (I am an expert)?")))
        (:div
         (#:how-often-talk-A
          (input-likert (format "How often do you talk about the topic with your family or colleagues on a scale from 1 (never) to 7 (all the time)?"))))
        submit-button)))))))

(define (thank-you)
  (page
   (haml
    (.container
     (:h1 "Thank you")

     (:p "Thank you for participating in our survey. Please provide the following completion code on prolific to receive your payment:")

     (:h4 completion-code)))))

(define topics
  (list
   "Gender"
   "Inequality"
   "Covid"
   "Artificial Intelligence (AI)"
   "Sports"
   "Climate Change"))

(define (randomization)
  (define ts (random-sample topics 2))
  (skip))

(define edpb-reasons-pilot
  (make-study
   "edpb-reasons-pilot"
   #:transitions
   (transition-graph
    [welcome-and-consent --> ,(lambda ()
                                (if (string=? (get 'consent?) "yes")
                                    (goto topic-survey-A)
                                    (goto no-consent)))]

    [topic-survey-A --> topic-survey-B --> thank-you --> thank-you]

    [no-consent --> no-consent])

   (list
    (make-step 'welcome-and-consent welcome-and-consent)
    (make-step 'randomization randomization)
    (make-step 'topic-survey-A
               (lambda ()
                 (topic-survey (get 'A))))
    (make-step 'topic-survey-B
               (lambda ()
                 (topic-survey (get 'B))))
    (make-step 'no-consent no-consent)
    (make-step 'thank-you thank-you))))
