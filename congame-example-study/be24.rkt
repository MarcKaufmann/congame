#lang conscript

(provide
 reading1-quiz)

(defstep (summary)
  @html{
        @form{
              @textarea[#:huber-summary]{Briefly summarize the Huber et al (2023) paper. Make sure to mention the difference between the organizing team of authors and the research teams.}
              @textarea[#:reproducibility-replicability]{In your own words, briefly describe what reproducibility and replicability mean.}
              @submit-button
              }
        })

(defstep (quiz)
  @html{
        @form{
              @input-number[#:n-research-teams]{How many research teams participated in the study until the end?}
              @input-number[#:replication-rate #:min 0 #:max 100]{The Dreber and Johannesson (2023) working paper mentioned the Reproducibility Project: Psychology (2015), a project that replicated 100 studies in psychology to see how many replicated. What was the replication rate of papers -- that is, how many of the 100 studies did replicate?}
              @input-text[#:research-question]{What was the common research question that all research teams tried to answer?}
              @input-number[#:n-participants]{How many participants did Huber et al (2023) roughly for all the studies?}
              @submit-button
              }
        })

(defstep (end)
  (define score
    (+ (equal? (get 'n-research-teams) 45)
       (< 5 (abs (- (get 'replication-rate) 35)))
       (< 1500 (abs (- (get 'n-participants) 18123)))
       (regexp-match? #rx".*(?i:c)ompetition.*(?i:m)oral.*" (get 'research-question))))

  @md{
      # Thanks for participating

      Here are the answers to the quiz question:

      - There were 45 research teams that participated in Huber et al (2023) until the end.
      - The replication rate in the Reproducibility Project: Psychology (2015) was 35%, i.e. 35 of the original 100 studies had a significant effect
      - The common research question in Huber et al (2023) was "Does competition affect moral behavior?"
      - The Huber et al (2023) study had somewhat more than 18,000 participants.

      You got @score of these right it seems.
      })

(defstudy reading1-quiz
  [summary --> quiz
           --> end]
  [end --> end])
