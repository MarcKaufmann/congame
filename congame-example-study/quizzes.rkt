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
 info-econ-quiz2
 info-econ-quiz3
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

     (:p (:strong "Your participant id: ") (~a (current-participant-id)))
     (:h3 (format "Total Score: ~a" total-score))
     (:p "Your score will be updated as your questions are graded")

     (:h3 "Overview")
     ,@(for/list ([(key answer) (in-hash (hash-ref (get/instance 'quiz-answers) (current-participant-id)))])
         (define q-score
           (quiz-question-score answer))
         (haml
          (:div.quiz
           (:ul
            (:li (:strong "Question: ") (quiz-question-text answer))
            (:li (:strong "Your Score: ") (if q-score (~a q-score) "(Not graded)"))
            (:li (:strong "Your Answer: ") (quiz-question-answer answer)))
           (if (get/instance 'phase-show-answers #f)
               (haml
                (:div
                 (:h4 "Suggested Answer")
                 (answer-haml-for key)))
               (haml
                (:div
                 (button
                  (λ ()
                    (put 'question-to-change (list (list key (quiz-question-text answer) ""))))
                  "Change your Answer"
                  #:to-step-id 'change-answer)))))))))))

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

  (define quiz-question-names
    (for/list ([q (get 'quiz-questions)])
      (car q)))

  (define all-answers
    (parameterize ([current-study-stack '(*root*)])
      (get/instance 'quiz-answers (hash))))

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
      (:table.quiz-answers
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
     ,@(for/list ([n quiz-question-names])
         (display-table n))

     (:h3 "Actions")
     (button
      (λ ()
        (parameterize ([current-study-stack '(*root*)])
          (put/instance 'phase-show-answers #t)))
      "Release Answers to Participants"
      #:to-step-id 'admin)
     (button
      (λ ()
        (parameterize ([current-study-stack '(*root*)])
          (put/instance 'phase-show-answers #f)))
      "Hide Answers from Participants"
      #:to-step-id 'admin)
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
    (displayln (car remaining-questions))
    (match-define (list next-key q-text _)
      (car remaining-questions))
    (define maybe-answer
      (parameterize ([current-study-stack '(*root*)])
        (define answer?
          (hash-ref (get 'quiz-answers (hash)) next-key #f))
        (if answer?
            (quiz-question-answer answer?)
            #f)))
    (page
     (haml
      (.container
       (:h3 "Question")
       (:p (:strong "Your participant id: ") (~a (current-participant-id)))
       (if maybe-answer
           (haml
            (:div
             (:p (:strong "Your previous answer: ") maybe-answer)))
           (haml
            (:div
             "")))
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
         'finished
         'question))))

(define (finished-all-questions)
  (page
   (haml
    (.container
     (:h2 "You finished/updated all questions")
     (button void "Continue")))))

(define quiz-study
  ;; Should check that loq not empty, that seems a mistake always
  (make-study
   "quiz-study-from-questions"
   #:provides '(quiz-answers)
   #:requires '(quiz-questions)
   (list
    (next-question-step)
    (make-step 'finished finished-all-questions))))

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
    (make-step/study
     'change-answer
     quiz-study
     #:provide-bindings '((changed-quiz-answers quiz-answers))
     #:require-bindings '((quiz-questions question-to-change))
     (λ ()
       (displayln (format "QUESTION TO CHANGE: ~a" (get 'changed-quiz-answers)))
       (define new-answers
         (for/fold ([new-quiz-answers (get 'quiz-answers)])
                   ([(key answer) (in-hash (get 'changed-quiz-answers))])
           (hash-set new-quiz-answers key answer)))
       (put 'quiz-answers new-answers)
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

(define info-econ-quiz2-questions
  (list
   (list
    'goods-market
    "For a goods economy (such as for airline seats or apples), how does private information by customers affect the outcome in a competitive market?"
    (haml
     (:p "We simply have a competitive market equilibrium, so there is no distortion compared to full information.")))
   (list
    'insurance-market
    "For an insurance market, how does private information by customers affect the outcome in a competitive market?"
    (haml
     (:p
      "While there might be a market in which the high and the low types are separated, there might not be any separating equilibrium. There also is no pooling equilibrium. Thus there might not be any pure strategy equilibrium!")))
   (list
    'difference-goods-vs-insurance-market
    "Why is there such a difference between the goods and the insurance market under competition but not for a monopoly? What is so different?"
    (haml
     (:p
      "The cost of producing an apple or providing an airplane seat is fixed and known: it does not depend on the type of customer that buys the apple or airplane. The cost of insurance on the other hand " (:emph "does") " depend on the type of customer: some customers are more likely to fall ill or become unemployed. But which customer buys from one insurance depends not only on the contract offered by the insurance itself, but also on what contract /other/ insurances offer, since another insurance might attract (via a profitable deviation) the more profitable type, leaving the other insurance only the 'bad' customers.")))
   (list
    'screening-with-two-types-step-2
    "Go to Step 2 of our derivation of the optimal screening contract. We claimed that we can increase \\(T\\) to \\(T + \\varepsilon\\) if the \\(IC_H\\) does not bind. But this might make the \\(IR_H\\) worse!

1. State what \\(IC_H\\) and \\(IR_H\\) mean, both what the acronyms stand for, then what this acronym means in terms of economics.
2. Did we make a mistake when we said that the firm can increase \\(T\\) to \\(T + \\varepsilon\\)? After all, by increasing \\(T\\) we might violate \\(IR_H\\). Why is this not a problem? (Hint: We would be in trouble if we had not yet done step 1.)"

    (haml
     (:p "See lecture notes for part 1. Part 2: Step 1 of the proof shows that as long as \\(IR_L\\) and \\(IC_H\\) hold, we know that \\(IR_H\\) holds. We assumed in Step 2 (to get started on our contradiction) that the \\(IC_H\\) does not bind. Therefore there is some \\(\\varepsilon\\) s.t. the \\(IC_H\\) still binds for \\(T + \\varepsilon\\). Moreover, setting the price to \\(T + \\varepsilon\\) does not affect \\(IR_L\\). Hence, both \\(IR_L\\) and \\(IC_H\\) still hold, so that by Step 1, \\(IR_H\\) also still holds.")))

   (list
    'competitive-insurance-indifference-curves
    "In the example for competitive insurance companies, consider the indifference curve \\(I_H\\) of the high type through some bundle \\(C\\) -- high type means the one with a higher probability of loss. Consider the indifference curve \\(I_L\\) of the low type through this same bundle \\(C\\). The x-axis has the amount the person gets in the no-loss state, the y-axis the amount in the loss state. Consider a bundle \\(C'\\) on \\(I_H\\) that lies to the left of \\(C\\) (i.e. it yields less consumption in the no-loss state). Draw this diagram, and send it to me as a png/pdf in a private message on Slack and include your participant number (see above). Provide the argument why the curves are as you drew them, and why \\(C'\\) lies above or below (choose the correct one) \\(I_L\\)."
    (haml
     (:p "See notes.")))

   (list
    'compute-screening-bundles
    "Consider a product that has only integer quality levels. There are two types of consumer: the high type values additional units of quality by 6, 5, 4, 3, 2, 1, and 0. That is, the first unit is worth 6, the second 5, and so on. The low type values units of quality by 4, 3, 2, 1, and 0. The marginal cost of increasing the quality of the product is 2.

What are the optimal bundles the firm should produce if the firm wants to serve both customers at the highest quality level? I only want to be given the two bundles, i.e. \\(x, y\\) and \\(z, w\\) where \\(x\\), \\(y\\), \\(z\\), and \\(w\\) are integers. Specify the quality first, then the price."
    (haml
     (:p "\\( (5, 18) \\) and \\( (1, 4) \\). Another bundle is \\( (5, 20) \\) and \\( (0, 0) \\), but this does not provide the highest quality to the low type.")))))

(define info-econ-quiz2
  (make-quiz-study info-econ-quiz2-questions))


(define info-econ-quiz3-questions
  (list
   (list
    'competitive-insurance-slope
    "Consider again the competitive insurance market. Suppose the low type has initial wealth \\(100\\) and a probability \\(p_L = 0.2\\) of losing \\(20\\) of their wealth. What is the slope of the zero-profit line when serving only low types? (The slope should be negative, since the curve is downward sloping.)"
    (haml
     (:p "At all points on the zero-profit line, the expected wealth of the low type is the same. The expected profit of the low type is \\(0.8 \\cdot 100 + 0.2 \\cdot (100 - 20) = 80 + 16 = 96 \\). Hence we want the line of \\(x, y\\) s.t. $0.8 x + 0.2 y = 96$. We can solve this to get $0.2 y = 96 - 0.8 x \\implies y = 96 \\cdot 5 - 4 x$, so the slope is $-4$.")))

   (list
    'sell-the-firm-moral-hazard
    "Consider moral hazard. Argue in words how selling the firm works when both the principal and agent are risk neutral."
    (haml (:p "See class notes")))

   (list
    'deterministic-output-moral-hazard
    "Suppose that output depends deterministically on effort -- i.e. there is no uncertainty about output conditional on effort. What is the optimal contract to induce high effort in that case under asymmetric information?"
    (haml
     (:p
      "If low effort leads to deterministically low output, and high effort to high output, then the firm can in fact contract on effort -- after all, contracting on output is the same as contracting on effort. Thus by paying a sufficient salary for the high output and a sufficiently low salary (possibly negative) for low output, the firm can ensure the high output.")))

   (list
    'tax-maxing-moral-hazard
    "Consider a government that wants to extract the maximum amount of tax. All its citizens are identical (!) and can exert high or low effort. If they exert high effort, they generate more income. The value of the outside option of not working at all (they emigrate) is 100 currency units. Low effort incurs a cost of 20 currency units, high effort a cost of 40 CI (currency units).

Suppose that under low effort the income is uniformly distributed on $[100, 150]$ and under high effort there is a 20% chance that income is below 25, in which case it is uniformly distributed on $[100,125]$; and there is an 80% chance that it is above 25, in which case it is uniformly distributed on $[125, 150]$."
    (haml
     (:p
      "Since the marginal cost of going from low to high effort is 20, and the marginal benefit is less (the expected income is 125 under low effort, and less than 137.5 under high effort), low effort will be optimal. Hence the government can simply allow the citizens to take away 120 currency units (their outside option plus cost of low effort) and take the rest. The government thus makes a revenue of 5 per citizen.")))))

(define info-econ-quiz3
  (make-quiz-study info-econ-quiz3-questions))
