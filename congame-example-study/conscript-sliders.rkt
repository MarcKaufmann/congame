#lang conscript

(require
 conscript/survey-tools)

(provide
 sliders)

(defstep (simple-slider)
  @md{
    # How to create sliders

    The easiest way to create sliders is by using `input-range`, which by default uses a range from 0 to 100:
    ```
    @"@"form{
      @"@"input-range[#:slider1]{Set the slider to exactly 25.}
      @"@"submit-button
    }
    ```

    This will display the following:

    @form{
      @input-range[#:slider1]{Set the slider to exactly 25.}
      @submit-button
    }})

(defstep (working-with-selected-value)
  (define slider-value
    (get 'slider1))
  (define slider-correct?
    (= 25 slider-value))
  (define bonus
    (if slider-correct? 1 0))
  (define message
    (if slider-correct? "right" "wrong"))
  @md{
    # Using the selected slider value

    We can get the value of the slider with `(get 'slider1)` - based on the name we provided in `@"@"input-range[#:slider1]{...}` - and then use this to check whether the person managed to set the slider at the value of 25 or not.

    Suppose that the person receives a bonus of $1 if they set the slider correctly at 25, then we can compute the bonus as follows:
    ```
    (define slider-value
      (get 'slider1))
    (define slider-correct?
      (= 25 slider-value))
    (define bonus
      (if slider-correct? 1 0))
    (define message
      (if slider-correct? "right" "wrong"))
    ```

    Finally we can display to the user whether they got the slider right and what bonus they get:

    ```
    You got the slider @message, so your bonus is @"@"~a[bonus].
    ```

    Note that we apply `@"@"~a` to `bonus` to convert it from a number to a string, otherwise it would not display properly.

    In your case, you got the slider @message, so your bonus is @~a[bonus].

    @button{Continue} })

(defstep (slider-display-value)
  @html{
    @slider-js

    @h1{Sliders with displayed values}

    You may want to display the currently selected value of the slider as the slider is moved. To do so, you need to include some Javascript code by writing `@"@"slider-js` at the top of the HTML. For that to work, you need to add `(require conscript/survey-tools)` at the top of your conscript file, which will provide you with the definition of the Javascript code.

    Finally, `slider-js` expects the HTML of the sliders to look as follows:

    @html*{
      @pre{
        @"@"form{
          @"@"div[#:class "slider"]{
            @"@"input-range[#:slider1] @"@"span{Value: @"@"output{}}
          }
          @"@"div[#:class "slider"]{
            @"@"input-range[#:slider2] @"@"span{Value: @"@"output{}}
          }
          @"@"submit-button
        }
      }
    }

    This will create the following form:

    @form{
      @div[#:class "slider"]{
        @input-range[#:slider1] @span{Value: @output{}}
      }
      @div[#:class "slider"]{
        @input-range[#:slider2] @span{Value: @output{}}
      }
      @submit-button
    }
  })

(define (easy-sliders)
  @html{
    @slider-js

    @h1{Easy Sliders}

    To create a list of sliders, just write `@"@"make-sliders[n]`, where `n` is the number of sliders you want in the form. Note that this form does not contain any other input fields. When creating a form with only a few sliders, you may therefore prefer to write them manually, but for creating 10 sliders, that is tedious.

    Here are 10 sliders:

    @make-sliders[10]
  })

(define (many-many-sliders-with-random-starting-point)
  (define (make-custom-slider _)
    (define start-value (~a (random 100)))
    @input-range[#:attributes `([value ,start-value])])

  @md{# More sliders than you can handle

      @make-sliders[10 make-custom-slider]
  })

(define (many-many-customized-sliders-with-increasing-starting-point)
  (define (make-custom-slider i)
    (define r (* 2 (random 50 100)))
    @input-range[
      #:max r
      #:attributes `([value ,(~a (* 10 i))]
                     [style ,(format "width: ~apx" r)])
    ])

  @md{# More sliders than you can handle

      @make-sliders[10 make-custom-slider]
  })


(define (end)
  @md{
    # The End

    If you want to restart this tutorial, click below.

    @button{Start from Beginning}})

(defstudy sliders
  [simple-slider --> working-with-selected-value
                 --> slider-display-value
                 --> easy-sliders
                 --> many-many-sliders-with-random-starting-point
                 --> many-many-customized-sliders-with-increasing-starting-point
                 --> end
                 --> simple-slider])
