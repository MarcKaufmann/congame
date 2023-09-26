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

(define payment 3.00)
(define completion-code "CET2506Q")
(define n-topics (length topics))
(define survey-duration 15)

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

(define (input-likert/how adjective)
  (input-likert (format "How ~a do you find the topic? (1: not at all. 7: extremely.)" adjective)))

(define ((topic-survey topic index p))
  (page
   (haml
    (.container
     (:h3 (format "Topic number ~a: ~a" index topic))

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
        (:div (#:knowledge
          (input-likert "How well do you know the topic? (1: never heard about it. 7: you are an expert.)")))
        (:div
         (#:how-often-talk-family
          (input-likert (format "How often do you talk about the topic with your family? (1: never. 7: all the time.)"))))
        (:div
         (#:how-often-talk-colleagues
          (input-likert (format "How often do you talk about the topic with colleagues? (1: never. 7: all the time.)"))))
        (:div
         (#:curious
          (input-likert (format "How curious are you about the topic? (1: not at all. 7: extremely, you would enroll in a course on the topic if you had time.)"))))
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
  (put 'topics (shuffle topics))
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
  (define (put/round k v)
    (put #:round (get-current-round-stack)
         #:group (get-current-group-stack)
         k v))

  (define (set-state! topics)
    (define topic (car topics))
    (define remaining-topics (cdr topics))
    (put-current-round-name topic)
    (put/loop 'topic topic)
    (put/loop 'index (- n-topics (length remaining-topics)))
    (put/loop 'remaining-topics remaining-topics))

  (define (setup)
    (set-state! (get 'topics))
    (skip))

  (define (loop)
    (define remaining-topics (get/loop 'remaining-topics))
    (cond [(empty? remaining-topics)
           (skip)]
          [else
           (set-state! remaining-topics)
           (skip 'maybe-attention-check)]))

  (define (maybe-attention-check)
    (define i (get/loop 'index))
    (cond [(and (> i 1) (zero? (modulo (sub1 i) 5)))
           (page
            (haml
             (.container
              (:h1 (format "You have completed ~a topics" (sub1 i)))

              (:p "If the questions are becoming monotonous - since we ask the same questions about all topics - take a few breaths to refocus before continuing.")

              (button void "Continue when ready"))))]
          [else
           (skip)]))

  (make-study
   "sequence of topic surveys"
   #:requires '(topics)
   #:transitions
   (transition-graph
    [setup-loop --> maybe-attention-check
                --> one-topic-survey
                --> loop
                --> ,(lambda () done)])
   (list
    (make-step 'setup-loop setup)
    (make-step 'maybe-attention-check maybe-attention-check)
    (make-step 'loop loop)
    (make-step 'one-topic-survey
               (lambda ()
                 ((topic-survey (get/loop 'topic) (get/loop 'index) put/round)))))))

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
