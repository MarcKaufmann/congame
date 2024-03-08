#lang conscript

(provide
 randomized-study)

(require
 conscript/survey-tools)

(define (randomize-treatments)
  (assigning-treatments
    (list 'control 'treatment)))

(defstep (control-page)
  @md{
    # Control Page

    You are in the control treatment.

    @button{Continue}})

(defstudy control-study
  [control-page --> ,(lambda () done)])

(defstep (treatment-page)
  @md{
    # Treatment Page

    You are in the treatment treatment. (I know.)

    @button{Continue}
})

(defstudy treatment-study
  [treatment-page --> ,(lambda () done)])

(defstep (welcome)
  @md{
    # Welcome

    Thank you for participating in this study.

    @button[randomize-treatments]{Continue}})

(define (final-page)
  (define participant-treatment
    (~a (get* 'role)))

  @md{
    # Final Page

    Thank you for having participated in the *@|participant-treatment|* treatment.})

(defstudy randomized-study
  [welcome --> ,(lambda ()
                   (case (get* 'role)
                     [(control)   'control-study]
                     [(treatment) 'treatment-study]))]

  [control-study --> final-page]
  [treatment-study --> final-page]
  [final-page --> final-page])
