#lang racket/base

(require congame/components/formular
         congame/components/study
         congame/components/export
         congame-web/components/identity
         koyo/haml
         racket/format
         racket/generic
         racket/match
         racket/serialize)

(provide
 info-econ-quiz1
 #;(struct-out quiz-question)
 )

(define ((start questions))
  (page
   (haml
    (.container
     (:h1 "Quiz")
     (:p "When ready, start the quiz")
     (button
      void
      "Start Quiz")))))

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

(define (get-answers/current-participant)
  (hash-ref
   (get/instance 'quiz-answers)
   (current-participant-id)))

(define (get-answer/current-participant key)
  (hash-ref (get-answers/current-participant) key #f))

(define (quiz-question-get-score key)
  (quiz-question-score
   (hash-ref
    (get-answers/current-participant)

    'key
    #f)))

(define (final-page)
  (define scores
    (for/list ([(key answer) (in-hash (hash-ref (get/instance 'quiz-answers) (current-participant-id)))])
      (quiz-question-score answer)))
  (define total-score
    (if (andmap number? scores)
        (apply + scores)
        "Not graded"))
  (put 'total-score total-score)
  (put/identity 'total-score total-score)

  (page
   (haml
    (.container
     (:h1 "Quiz Completed")

     (:h3 (format "Total Score: ~a" total-score))
     (:p "Your score will be updated as your questions are graded")

     (:h3 "Answers")
     ,@(for/list ([(key answer) (in-hash (hash-ref (get/instance 'quiz-answers) (current-participant-id)))])
         (define q-score
           (quiz-question-score answer))
         (haml
          (:div.quiz
           (:ul
            (:li (:strong "Question: ") (quiz-question-text answer))
            (:li (:strong "Your Score: ") (if q-score (~a q-score) "Not graded)")))
           (:h4 "Suggested Answer")
           (answer-haml-for key))))))))

(define (question-for key)
  (cdr (findf (λ (q)
                (equal? (car q) key))
              (get 'quiz-questions))))

(define (answer-haml-for key)
  (cadr (question-for key)))

(define (question-haml-for key)
  (haml
   (:p (car (question-for key)))))

(define (quiz-admin-handler)

  (define all-answers
    (parameterize ([current-study-stack '(*root*)])
      (get/instance 'quiz-answers '())))

  (define (all-answers-for key)
    (for/hash ([(id answers) (in-hash all-answers)]
               #:when (hash-has-key? answers key))
      (values id (hash-ref answers key))))

  (define (display-table key)

    (define (update-score p new-score)
      (define current-participant-answers
        (hash-ref all-answers p))
      (define new-answers
        (hash-update current-participant-answers
                     key
                     (λ (q)
                       (quiz-question
                        (quiz-question-text q)
                        (quiz-question-answer q)
                        new-score))))
      (parameterize ([current-study-stack '(*root*)])
        (put/instance
         'quiz-answers
         (hash-set (get/instance 'quiz-answers (hash))
                   p
                   new-answers))))

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
       ,@(for/list ([(p q) (in-hash (all-answers-for key))])
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
   #:requires '(quiz-questions)
   (list
    (make-step 'admin quiz-admin-handler))))

(define (next-question-step)
  (define (handler)
    (define remaining-questions (get 'quiz-questions))
    (match-define (list next-key q-text _)
      (car remaining-questions))
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
            (put 'quiz-answers
                 (hash-set (get 'quiz-answers (hash)) next-key q))
            (put 'quiz-questions-done
                 (cons (car remaining-questions) (get 'quiz-questions-done '())))
            (put 'quiz-questions (cdr remaining-questions)))))))))
  (make-step
   'question
   handler
   (λ ()
     (if (null? (get 'quiz-questions))
         done
         'question))))

(define quiz-study
  ;; Should check that loq not empty, that seems a mistake always
  (make-study
   "quiz-study-from-questions"
   #:provides '(quiz-answers)
   #:requires '(quiz-questions)
   (list
    (next-question-step))))

(define (make-quiz-study questions)
  (make-study
   "quiz-study"
   #:provides '()
   #:requires '()
   (list
    (make-step
     'check-owner
     skip
     (λ ()
       (put 'quiz-questions questions)
       (if (current-participant-owner?) 'quiz-admin 'start)))
    (make-step 'start (start info-econ-quiz1-questions))
    (make-step/study
     'quiz
     quiz-study
     #:provide-bindings '((quiz-answers quiz-answers))
     #:require-bindings '((quiz-questions quiz-questions))
     (λ ()
       (put/instance
        'quiz-answers
        (hash-set
         (get/instance 'quiz-answers (hash))
         (current-participant-id)
         (get 'quiz-answers)))
       'final-page))
    (make-step 'final-page final-page)
    (make-step/study
     'quiz-admin
     quiz-admin/study
     #:require-bindings '((quiz-questions quiz-questions))
     (λ ()
       'admin-interface)))))

;;; Info Econ Quiz 1

(define info-econ-quiz1-questions
  (list
   (list
    'comparative-statics
    "What does Comparative Statics mean? Explain and provide an example."
    (haml
     (:div
      (:p "Comparative Statics study how some endogenous variable Y changes in equilibrium with the change in an exogenous variable X.")
      (:p "Examples")
      (:ul
       (:li "How does the price of a good (endogenous) change following an increase in its production cost (assumed exogenous)?")
       (:li "How does the wage of a worker change after a raise in the minimum wage?")))))
   (list
    'first-wft
    "What does the First Welfare Theorem say?"
    (haml
     (:p "There are different variations, but it says roughly that if markets are complete (a price for every good in every state), are competitive (no market power, hence price-taking), and buyers and sellers are rational and fully informed, then the market is Pareto efficient.")))
   (list
    'different-patience
    "Suppose that the difference in utility derives from one type being more impatient than the other. The firm is selling exactly the same product, but offers it in period 1 and period 2 for different prices. Which is the higher type: the more patient or the less patient person? Explain what defines the higher type."
    (haml
     (:p "The high type is the one who values " (:strong "increase") " in quality more and thus is willing to pay more for such increases. The more impatient person is the one willing to pay more to receive the good in period 1, hence they are the high type. (This does rely somewhat on the assumption that both types value no bundle or no quantity the same amount -- that they have similar outside options.)")))
   (list
    'information-rent
    "Suppose that the bundles we observe are (T, q) and (T', q'), with (T, q) being for the higher type. Expressed in terms of utility of these bundles, how much information rent can the highest type make at most? What about the lowest type? Why? (Note: as in the lecture notes, we implicitly assume that each type can buy only one bundle or the other, but not both or multiples of each bundle.)"
    (haml
     (:p "The lowest type never gets any information rent. Letting u be the utility of the highest type, the highest type can pretend to be the lowest type to receive u(q') - T' from pretending to be the low type.")))))

(define info-econ-quiz1
  (make-quiz-study info-econ-quiz1-questions))
