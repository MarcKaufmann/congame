#lang racket/base

(require congame/components/formular
         congame/components/study
         congame/components/export
         koyo/haml
         racket/format
         racket/generic
         racket/match
         racket/serialize)

(provide
 info-econ-quiz1
 #;(struct-out quiz-question)
 )

(define (start)
  (page
   (haml
    (.container
     (:h1 "Quiz")
     (:p "When ready, start the quiz")
     (button void "Start Quiz")))))


#;(serializable-struct quiz-question (text answer score)
                     #:transparent
                     #:methods gen:jsexprable
                     [(define (->jsexpr q)
                        #;(match-define (quiz-question text answer score) q)
                        "test"
                        #;(hash 'type "quiz-question"
                              'text text
                              'answer answer
                              'score score))])

(define (quiz-question t a sc)
  (hash 'text t
        'answer a
        'score sc))

(define (quiz-question-text q)
  (hash-ref q 'text))
(define (quiz-question-answer q)
  (hash-ref q 'answer))
(define (quiz-question-score q)
  (hash-ref q 'score))

(define (make-quiz-question text answer)
  (quiz-question text answer #f))

(define ((question-handler key))
  (define q-text (question-for key))
  (page
   (haml
    (.container
     (:h3 "Question")
     (formular
      (haml
       (:div
        (#:answer (input-textarea q-text))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:answer answer)
        (let ([q (make-quiz-question q-text answer)])
          (put key q)
          (put/instance
           key
           (hash-set
            (get/instance key (hash))
            (current-participant-id)
            q)))))))))

(define (quiz-question-get-score key)
  (quiz-question-score
   (hash-ref
    (get/instance key)
    (current-participant-id))))

(define (done)
  (define scores
    (for/list ([key (hash-keys info-econ-quiz1-questions)])
      (quiz-question-get-score key)))
  (define total-score
    (if (andmap number? scores)
        (apply + scores)
        "Not graded"))
  (page
   (haml
    (.container
     (:h1 "Quiz Completed")

     (:h3 (format "Total Score: ~a" total-score))
     (:p "Your score will be updated as your questions are graded")

     (:h3 "Answers")
     ,@(for/list ([key (hash-keys info-econ-quiz1-questions)])
         (define q-score
           (quiz-question-get-score key))
         (haml
          (:div.quiz
           (:h4 "Question")
           (:p (question-for key))
           (:h4 (format "Your Score: ~a"
                        (if q-score q-score "Not graded")))
           (:h4 "Answer")
           (answer-haml-for key))))))))

(define info-econ-quiz1-questions
  (hash
   'comparative-statics
   (cons
     "What does Comparative Statics mean? Explain and provide an example."
    (haml
     (:div
      (:p "Comparative Statics study how some endogenous variable Y changes in equilibrium with the change in an exogenous variable X.")
      (:p "Examples")
      (:ul
       (:li "How does the price of a good (endogenous) change following an increase in its production cost (assumed exogenous)?")
       (:li "How does the wage of a worker change after a raise in the minimum wage?")))))
   'first-wft
   (cons
     "What does the First Welfare Theorem say?"
    (haml
     (:p "There are different variations, but it says roughly that if markets are complete (a price for every good in every state), are competitive (no market power, hence price-taking), and buyers and sellers are rational and fully informed, then the market is Pareto efficient.")))))

(define (question-for key)
  (car (hash-ref info-econ-quiz1-questions key)))

(define (question-haml-for key)
  (haml
   (:p
    (car (hash-ref info-econ-quiz1-questions key)))))

(define (answer-haml-for key)
  (cdr (hash-ref info-econ-quiz1-questions key)))

(define (quiz-admin-handler)

  (define (display-table key)
    (define answers
      (parameterize ([current-study-stack '(*root*)])
        (get/instance key (hash))))
    (define (update-score p new-score)
      (parameterize ([current-study-stack '(*root*)])
        (put/instance
         key
         (hash-update answers p
                      (λ (q)
                        (quiz-question
                         (quiz-question-text q)
                         (quiz-question-answer q)
                         new-score))))))
    (define (mark-as-correct p)
      (update-score p 1))
    (define (mark-as-incorrect p)
      (update-score p 0))


    (haml
     (:div
      (:h3 (format "Question ~a" key))
      (question-haml-for key)
      (:table
       (:thead
        (:tr
         (:th "Participant")
         (:th "Answer")
         (:th "Score")
         (:th "(Re)Grade")))
       ,@(for/list ([(p q) (in-hash answers)])
           (haml
            (:tr
             (:td (~a p))
             (:td (~a (quiz-question-answer q)))
             (:td (~a (if (quiz-question-score q)
                          (quiz-question-score q)
                          "")))
             (:td
              (button
               (λ ()
                 (mark-as-correct p))
               "Correct"
               #:to-step-id 'admin)
              (button
               (λ ()
                 (mark-as-incorrect p))
               "Incorrect"
               #:to-step-id 'admin)))))))))

  (page
   (haml
    (.container
     (:h1 "Quiz Admin")
     (display-table 'comparative-statics)
     (display-table 'first-wft)
     ))))

(define quiz-admin/study
  (make-study
   "quiz-admin"
   #:provides '()
   #:requires '()
   (list
    (make-step 'admin quiz-admin-handler))))

(define info-econ-quiz1
  (make-study
   "info-econ-quiz1"
   #:provides '()
   #:requires '()
   (list
    (make-step
     'check-owner
     skip
     (λ ()
       (if (current-participant-owner?) 'quiz-admin 'start)))
    (make-step 'start start)
    (make-step 'question1 (question-handler 'comparative-statics))
    (make-step 'question2 (question-handler 'first-wft))
    (make-step 'done done)
    (make-step/study
     'quiz-admin
     quiz-admin/study
     (λ ()
       'admin-interface)))))
