#lang conscript

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
    You got the slider @message, so your bonus is @|bonus|.
    ```

    Note that we have to put |'s around the last bonus, otherwise the code refers to the varialbe named 'bonus.' not to the variable 'bonus' due to the dot ('.') at the end.

    In your case, you got the slider @message, so your bonus is @|bonus|.

    @button{Continue} })

(define (end)
  @md{
    # The End

    If you want to restart this tutorial, click below.

    @button{Start from Beginning}})

(defstudy sliders
  [simple-slider --> working-with-selected-value
                 --> end
                 --> simple-slider])
