#lang racket/base

(require racket/random
         racket/list
         congame/components/formular
         congame/components/study
         congame/components/transition-graph
         (submod congame/components/formular tools)
         koyo/haml)

(provide
 edpb-reasons-pilot)

(define payment 2.00)
(define completion-code "CET2506Q")
(define n-topics 6)
(define survey-duration 10)

(define (welcome-and-consent)
  (page
   (haml
    (.container
     (:h1 "Welcome")

     (:p (format "The following short survey asks your opinion about, and familiarity with, ~a specific topics. It takes around ~a minutes to complete, and you will receive ~a GBP as a completion bonus. Please answer the questions truthfully, as they provide valuable information to us for another study. We will not reveal your individual answers to the participants of the subsequent study. Instead, we will show them statistics based on the responses of all participants in this survey. We will not be able to link your answers and your identity beyond the Prolific ID."
                 n-topics
                 survey-duration
                 payment))

     (:p "If you agree to participate in the survey, you can still opt out anytime. However, you only receive the completion bonus if you complete the whole survey.")

     (formular
      (haml
       (:div
        (:div
         (#:prolific-id
          (input-text "Please provide your Prolific ID.")))

        (:div
         (#:consent?
          (radios "Do you agree to participate in the survey?"
                  '(("yes" . "Yes")
                    ("no"  . "No"))))
         submit-button))))))))

(define (no-consent)
  (page
   (haml
    (.container
     (:h1 "Thank you")

     (:p "You did not agree to participate in the survey. Thank you for taking the time to consider it.")))))

(define (input-likert label)
  #;(input-number label #:min 1 #:max 7)
  (select label
          `((""  . "--Please choose an option--")
            ("1" . " 1 ")
            ("2" . " 2 ")
            ("3" . " 3 ")
            ("4" . " 4 ")
            ("5" . " 5 ")
            ("6" . " 6 ")
            ("7" . " 7 "))))

(define (input-likert/how adjective)
  (input-likert (format "How ~a do you find the topic? (1: not at all. 7: extremely.)" adjective)))

(define ((topic-survey topic p))
  (page
   (haml
    (.container
     (:h3 topic)

     (:p (hash-ref topic-descriptions topic) " Please answer the following questions for this topic.")

     (formular
      (haml
       (:div
        (:div
         (#:interesting (input-likert/how "interesting")))
        (:div
         (#:important (input-likert/how "important")))
        (:div
         (#:controversial (input-likert/how "controversial")))
        (:div
         (#:understandable (input-likert/how "easy to understand")))
        (:div (#:familiar
          (input-likert "How familiar are you with the topic? (1: never heard about it. 7: you are an expert.)")))
        (:div
         (#:how-often-talk-family
          (input-likert (format "How often do you talk about the topic with your family? (1: never. 7: all the time.)"))))
        (:div
         (#:how-often-talk-colleagues
          (input-likert (format "How often do you talk about the topic with colleagues? (1: never. 7: all the time.)"))))
        (:div
         (#:learn-more
          (input-likert (format "How interested would you be in learning more about the topic? (1: not at all. 7: extremely, you would enroll in a course on the topic if you had time.)"))))
        (:div
         (#:heard-of-last-week
          (radios "Have you heard about the topic in the last week (the last 7 days)?"
                  '(("yes" . "Yes")
                    ("no"  . "No")))))
        submit-button))
      (put-form/with p))))))

(define (thank-you)
  (page
   (haml
    (.container
     (:h1 "Thank you")

     (:p "Thank you for participating in our survey. Please provide the following completion code on prolific to receive your payment:")

     (:h4 completion-code)))))

(define topics
  (list
   "Gender Inequality"
   "Socioeconomic Inequality"
   "Neuroscience"
   "Sports"
   "AI"
   "Covid"
   "Environment"
   "Addiction"
   "Health"
   "Cognitive Skills"
   "Self-control"
   "Politics"))

(define topic-descriptions
  (hash
   "Gender Inequality" "The topic is 'Gender Inequality', which refers to the unfair differences in opportunities, resources, and treatment based on someone's gender, often leading to disadvantages for certain genders."
   "Socioeconomic Inequality" "The topic is 'Socioeconomic Inequality', which means differences in opportunities, money, and resources among people in a society due to factors like income, education, and social background."
   "Neuroscience" "The topic is 'Neuroscience', which is the scientific study of the nervous system, exploring how the brain and body interact to influence behavior and cognition."
   "Sports" "The topic is 'Sports', encompassing physical activities and games involving skill, competition, and athleticism, often for entertainment and fitness."
   "AI" "The topic is 'AI' or Artificial Intelligence, involving the development of computer systems capable of performing tasks that typically require human intelligence."
   "Banking" "The topic is 'Banking', involving financial institutions and processes that manage money, offering services like saving, borrowing, and investing."
   "Covid" "The topic is 'Covid', referring to the highly contagious respiratory illness caused by the novel coronavirus SARS-CoV-2."
   "Environment" "The topic is 'Environment', encompassing the natural world and surroundings in which organisms live, including ecosystems and ecological health."
   "Addiction" "The topic is 'Addiction', involving physiological and psychological dependence on substances or behaviors, often leading to negative consequences."
   "Health" "The topic is 'Health', encompassing the overall physical, mental, and social well-being of an individual, including disease prevention and treatment."
   "Cognitive Skills" "The topic is 'Cognitive Skills', referring to mental abilities such as thinking, learning, memory, problem-solving, and decision-making."
   "Self-control" "The topic is 'Self-control', involving the ability to regulate one's own emotions, behaviors, and impulses in order to achieve long-term goals"
   "Politics" "The topic is 'Politics', involving activities and policies related to governance, power, and decision-making within a society."))

(define (randomization)
  (put 'topics (random-sample topics n-topics #:replacement? #f))
  (skip))

(define (topic-surveys)
  (define (get/loop k)
    (get #:root '*loop*
         #:round (get-current-round-stack)
         #:group (get-current-group-stack)
         k))
  (define (put/loop k v)
    (put #:root '*loop*
         #:round (get-current-round-stack)
         #:group (get-current-group-stack)
         k v))

  (define (set-state! topics)
    (define topic (car topics))
    (put-current-round-name topic)
    (put/loop 'topic topic)
    (put/loop 'remaining-topics (cdr topics)))

  (define (setup)
    (set-state! (get 'topics))
    (skip))

  (define (loop)
    (define remaining-topics (get/loop 'remaining-topics))
    (cond [(empty? remaining-topics)
           (skip)]
          [else
           (set-state! remaining-topics)
           (skip 'one-topic-survey)]))

  (make-study
   "sequence of topic surveys"
   #:requires '(topics)
   #:transitions
   (transition-graph
    [setup-loop --> one-topic-survey
                --> loop
                --> ,(lambda () done)])
   (list
    (make-step 'setup-loop setup)
    (make-step 'loop loop)
    (make-step 'one-topic-survey
               (lambda ()
                 ((topic-survey (get/loop 'topic) put/loop)))))))

(define edpb-reasons-pilot
  (make-study
   "edpb-reasons-pilot"
   #:transitions
   (transition-graph
    [welcome-and-consent --> ,(lambda ()
                                (if (string=? (get 'consent?) "yes")
                                    (goto randomization)
                                    (goto no-consent)))]

    [randomization --> topic-surveys --> thank-you --> thank-you]

    [no-consent --> no-consent])

   (list
    (make-step 'welcome-and-consent welcome-and-consent)
    (make-step 'randomization randomization)
    (make-step/study
     'topic-surveys
     (topic-surveys)
     #:require-bindings `([topics ,(lambda () (get 'topics))]))
    (make-step 'no-consent no-consent)
    (make-step 'thank-you thank-you))))
