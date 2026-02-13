#lang conscript

(provide
 radio-button-baby)

(require conscript/form0)

(defvar choice)
(defvar shuffled-options)

(defstep (two-buttons)
  (when (undefined? shuffled-options)
    (set! shuffled-options #f))
  (define options
    '(("1" . "SS")
      ("2" . "AD")))

  (set! shuffled-options
        (if (not shuffled-options) (shuffle options) shuffled-options))
  
  #;(define choices
      (for/list ([option shuffled-options]
                 [label '("Choice A" "Choice B")])
        (cons (car option) ; this is 1 or 2
              (format "~a: ~a" label (cdr option))))) ; "Choice A: SS" etc.
  
  (define-values (survey-form on-submit)
    (form+submit
     [choice (ensure binding/text (required))]))

  ; If there are more than 3 options, you should do render via for-loop.
  (define first-option (first shuffled-options))
  (define second-option (second shuffled-options))
  
  (define (render rw)
    @md{
      @style{
        .choices {
          display: flex;
          justify-content: center;
          gap: 40px;
        }
        .choices > div {
          display: flex;
          align-items: center;       /* keeps radio aligned with text */
          gap: 10px;
        }
        /* Optional tweaks */
        input[type="radio"] {
          transform: scale(1.2);
          cursor: pointer;
        }
      }
      @div[#:class "choices"
           @div[
                @rw["choice" @radio[@(car first-option) ""]]
                @(div
                  #:class "option-text"
                  @md*{**Investment A**

                    @(cdr first-option)
                    })]

           @div[
                @rw["choice" @radio[@(car second-option) ""]]
                @(div
                  #:class "option-text"
                  @md*{**Investment B**

                    @(cdr second-option)})]]
      @rw["choice" errors]

      @|submit-button|
      })
  
  @md{# The Form

    @form[survey-form on-submit render]})

(defstep (the-end)
  @md{# The End

    @button{Restart and Reshuffle}})

(defstudy radio-button-baby
  [two-buttons --> the-end]
  [the-end --> ,(lambda ()
                  (set! shuffled-options #f)
                  'two-buttons)])