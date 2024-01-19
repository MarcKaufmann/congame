#lang conscript

(require racket/format)

(provide
 reading1-quiz)

(defstep (summary)
  @html{
        @form{
              @textarea[#:huber-summary]{Briefly summarize the Huber et al (2023) paper. Make sure to mention the difference between the organizing team of authors and the research teams.}
              @submit-button
              }
        })

(defstep (quiz)
  @html{
        @form{
              @input-number[#:n-research-teams]{How many research teams participated in the study until the end?}
              @submit-button
              }
        })

(defstep (end)
  ; Inefficient since it runs on each refresh.
  #;(define score
    (apply
     +
     (map (lambda (x)
            (if x 1 0))
          (list
           (equal? (get 'n-research-teams) 45)
           (> 5 (abs (- (get 'replication-rate) 35)))
           (> 1500 (abs (- (get 'n-participants) 18123)))
           (regexp-match? #rx".*(?i:c)ompetition.*(?i:m)oral.*" (get 'research-question))))))

  #;(put 'score score)

  @html{
      @h1{Thanks for participating}

      Here are the answers to the quiz question:

      @ul{
          @li{There were 45 research teams that participated in Huber et al (2023) until the end.}
          @li{The replication rate in the Reproducibility Project: Psychology (2015) was 35%, i.e. 35 of the original 100 studies had a significant effect}
          @li{The common research question in Huber et al (2023) was "Does competition affect moral behavior?"}
          @li{The Huber et al (2023) study had somewhat more than 18,000 participants.}}

      })

(defstudy reading1-quiz
  [summary --> [summary1 summary]
           --> [summary2 summary]
           --> [summary3 summary]
           --> [summary4 summary]
           --> [summary5 summary]
           --> [summary6 summary]
           --> [summary8 summary]
           --> [summary9 summary]
           --> [summary10 summary]
           --> [summary11 summary]
           --> [summary12 summary]
           --> [summary13 summary]
           --> [summary14 summary]
           --> [summary15 summary]
           --> [summary16 summary]
           --> [summary17 summary]
           --> [summary18 summary]
           --> [summary19 summary]
           --> quiz
           --> [quiz2 quiz]
           --> [quiz3 quiz]
           --> [quiz4 quiz]
           --> [quiz5 quiz]
           --> [quiz6 quiz]
           --> [quiz7 quiz]
           --> [quiz8 quiz]
           --> [quiz9 quiz]
           --> end]
  [end --> end])
