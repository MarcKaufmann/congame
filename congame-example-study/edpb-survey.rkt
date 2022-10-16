#lang racket/base

(require congame/components/study
         congame/components/formular
         koyo/haml
         racket/format
         racket/hash)

(provide edpb-survey)

(define (put-self+instance k v)
  (put k v)
  (put/instance k
                (cons v (get/instance k '()))))

(define (open-feedback)
  (page
   (haml
    (.container
     (:h3 "Open-ended feedback")
     (formular
      (haml
       (:div
        (:div
         (#:plausible-explanation (input-textarea "What do you think explains both the significant result and the failed replication?")))
        (:div
         (#:other-explanations (input-textarea "What do you think are some other (less likely but possible) explanations?")))
        (:div
         (#:other-excuses (input-textarea "What else can serve as an excuse (whether other alternatives, or other treatments)?")))
        (:button.button.next-button ([:type "submit"]) "Next")))
      (lambda (#:plausible-explanation plausible-explanation
               #:other-explanations other-explanations
               #:other-excuses other-excuses)
        (put-self+instance 'plausible-explanation plausible-explanation)
        (put-self+instance 'other-explanations other-explanations)
        (put-self+instance 'other-excuses other-excuses)))))))

(define (subjective-beliefs)
  (define (input-sb label)
    (input-number (format "What is your subjective belief (in %) that ~a?" label)
                  #:min 0 #:max 100))
  (page
   (haml
    (.container
     (:h3 "Subjective Beliefs")
     (formular
      (haml
       (:div
        #;(:div
         (#:sb-pb (input-sb "present bias is real")))
        #;(:div
         (#:sb-excuses-increase-pb (input-sb "excuses increase present bias")))
        #;(:div
         (#:sb-our-design-elicits-edpb (input-sb "our design elicits excuse-driven present bias")))
        #;(:div
         (#:sb-replicable-but-not-excuses (input-sb "we are picking up something replicabel but it is not excuses")))
        (:div
         (#:sb-lucky-first-run (input-sb "we got 'lucky' in the first run and got a spurious result i.e. the null result is real")))
        (:button.button.next-button ([:type "submit"]) "Next")))
      (lambda (;#:sb-pb sb-pb
               ;#:sb-excuses-increase-pb sb-excuses-increase-pb
               ;#:sb-replicable-but-not-excuses sb-replicable-but-not-excuses
               ;#:sb-our-design-elicits-edpb sb-our-design-elicits-edpb
               #:sb-lucky-first-run sb-lucky-first-run
               )
        ;(put-self+instance 'sb-pb sb-pb)
        ;(put-self+instance 'sb-excuses-increase-pb sb-excuses-increase-pb)
        ;(put-self+instance 'sb-replicable-but-not-excuses sb-replicable-but-not-excuses)
        ;(put-self+instance 'sb-our-design-elicits-edpb sb-our-design-elicits-edpb)
        (put-self+instance 'sb-lucky-first-run sb-lucky-first-run)))))))

(define (done)
  (page
   (haml
    (.container
     (:h1 "Thanks for your feedback")))))

(define (admin)
  (define (mean k)
    (define v (get/instance k '()))
    (if (not (null? v))
        (~r (/ (apply + v) (* 1.0 (length v))) #:precision '(= 2))
        "no answers found"))

  (define (display-answers k title)
    (haml
     (:div
      (:h3 title)
      (:ul
       ,@(for/list ([a (get/instance k '())])
           (haml
            (:li a)))))))
  (page
   (haml
    (.container
     (:h1 "Admin Page")
     (:ul
      ;(:li (format "Mean belief 'present bias is real': ~a " (mean 'sb-pb)))
      ;(:li (format "Mean belief 'excuses increase present bias': ~a " (mean 'sb-excuses-increase-pb)))
      ;(:li (format "Mean belief 'replicable but not excuses': ~a " (mean 'sb-replicable-but-not-excuses)))
      ;(:li (format "Mean belief 'our design elicits excuses': ~a " (mean 'sb-our-design-elicits-edpb)))
      (:li (format "Mean belief 'lucky first run': ~a " (mean 'sb-lucky-first-run)))

      (display-answers 'plausible-explanation "Most Plausible Explanation")
      (display-answers 'other-explanations "Other Explanations")
      (display-answers 'other-excuses "Other Excuses"))))))

(define edpb-survey
  (make-study
   "EDPB-survey"
   (list
    (make-step
     'check-owner
     skip
     (λ ()
       (if (current-participant-owner?) 'admin 'open-ended-feedback)))
    (make-step 'open-ended-feedback open-feedback)
    (make-step 'subjective-beliefs-on-outcomes subjective-beliefs)
    (make-step 'done done)
    (make-step 'admin admin))))

;;;; Survey for briq lecture 2022

(provide briq-study)

(define (briq-feedback)
  (page
   (haml
    (.container
     (:h1 "Excuse Me")
     (formular
      (haml
       (:div
        (#:thoughts-on-excuses (input-textarea "How would you identify excuses? Which paper comes to mind?"))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:thoughts-on-excuses thoughts-on-excuses)
        (put-self+instance 'thoughts-on-excuses thoughts-on-excuses)))))))

(define (briq-admin)

  (define (mean k)
    (define v (get/instance k '()))
    (if (not (null? v))
        (~r (/ (apply + v) (* 1.0 (length v))) #:precision '(= 2))
        "no answers found"))

  (define lepper-reveal-aligned
    (get/instance 'predict-reveal-when-aligned '()))
  (define lepper-hist
    (for/fold ([r (hash)])
              ([l lepper-reveal-aligned])
      (hash-update r l add1 1)))
  (define lepper-not-revealed
    (mean 'predict-not-reveal-percentage))

  (page
   (haml
    (.container
     (:h1 "Admin")
     (:h3 "Lepper 2022")
     (:p "Predictions how people who revealed in the aligned state compared to people who knew they were aligned from the start:")
     (:ul
      (:li (format "More: ~a" (hash-ref lepper-hist "more" 0)))
      (:li (format "Same: ~a" (hash-ref lepper-hist "same" 0)))
      (:li (format "Less: ~a" (hash-ref lepper-hist "less" 0))))

     (:p (format "Mean predictions for % choosing less work when not revealing: ~a%" lepper-not-revealed))

     (:h3 "Shafir, Simonson, and Tversky (1993)")

     (:p (format "Mean prediction of % buying when they know they passed: ~a%" (mean 'predicting-buying-knowing-passed)))
     (:p (format "Mean prediction of % buying when they know they failed: ~a%" (mean 'predicting-buying-knowing-failed)))

     (button
      (λ ()
        (put/instance 'progress 'none))
      "Reset progress"
      #:to-step-id 'admin)
     (button
      (λ ()
        (put/instance 'progress 'lepper))
      "Move to Lepper (2022)"
      #:to-step-id 'admin)
     (button
      (λ ()
        (put/instance 'progress 'shafir))
      "Move to Shafir et al (1993)"
      #:to-step-id 'admin)))))

(define (done-page)
  (page
   (haml
    (.container
     (:h1 "Thank You!")
     (:p "Thanks for participating.")))))

(define (predicting-lepper)
  (page
   (haml
    (.container
     (:h1 "Predicting Lepper (2022)")
     (formular
      (haml
       (:div
        (:div
         (#:predict-reveal-when-aligned
          (radios
           "Consider people in the 'Excuse' treatment who choose to reveal the information and are in the aligned state. How do you think their answer compares to that of the people in the 'No Excuse' treatment who know that they are in the aligned state?"
           '(("more" . "They are more likely to pick 'Less work now'")
             ("same" . "They are just as likely to pick 'Less work now'")
             ("less" . "They are less likely to pick 'Less work now'")))))
        (:div
         (#:predict-not-reveal-percentage
          (input-number "Now consider the people in the 'Excuse' treatment who choose NOT to reveal the information. What percentage of these people do you think choose to do 'less work now'?"
                        #:min 0 #:max 100)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:predict-reveal-when-aligned predict-reveal-when-aligned
          #:predict-not-reveal-percentage predict-not-reveal-percentage)
        (put-self+instance 'predict-reveal-when-aligned predict-reveal-when-aligned)
        (put-self+instance 'predict-not-reveal-percentage predict-not-reveal-percentage)))))))

(define (predicting-shafir)
  (page
   (haml
    (.container
     (:h1 "Predicting Shafir, Simonson, and Tversky (1993)")
     (formular
      (haml
       (:div
        (:div
         (#:predicting-buying-knowing-passed
          (input-number "What is the percentage of people who chose to 'buy' when they knew they had passed the exam?"
                        #:min 0 #:max 100)))
        (:div
         (#:predicting-buying-knowing-failed
          (input-number "What is the percentage of people who chose to 'buy' when they knew they had failed the exam?"
                        #:min 0 #:max 100)))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (λ (#:predicting-buying-knowing-passed predicting-buying-knowing-passed
          #:predicting-buying-knowing-failed predicting-buying-knowing-failed)
        (put-self+instance 'predicting-buying-knowing-passed predicting-buying-knowing-passed)
        (put-self+instance 'predicting-buying-knowing-failed predicting-buying-knowing-failed)))))))

(define (wait-page)
  (page
   (haml
    (.container
     (:h1 "Please wait until the next question is released")
     (:p "Refresh the page once the next question is released.")))))


(define (continue-when-lepper)
  (define progress (get/instance 'progress 'none))
  (case progress
    [(none) (wait-page)]
    [(lepper shafir) (skip)]))

(define (continue-when-shafir)
  (define progress (get/instance 'progress 'none))
  (case progress
    [(none lepper) (wait-page)]
    [(shafir) (skip)]))

(define briq-study
  (make-study
   "briq-study"
   (list
    (make-step
     'check-owner
     skip
     (λ ()
       (if (current-participant-owner?) 'admin 'open-ended-feedback)))
    (make-step 'admin briq-admin)
    (make-step 'open-ended-feedback briq-feedback)
    (make-step 'continue-when-lepper continue-when-lepper)
    (make-step 'predicting-lepper predicting-lepper)
    (make-step 'continue-when-shafir continue-when-shafir)
    (make-step 'predicting-shafir predicting-shafir)
    (make-step 'done done-page))))
