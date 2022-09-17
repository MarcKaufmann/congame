#lang racket/base

(require congame/components/study
         congame/components/formular
         koyo/haml
         racket/format)

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
