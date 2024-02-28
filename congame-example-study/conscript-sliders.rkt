#lang conscript

(require
 conscript/utils)

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
  @md{
    @script{
      document.addEventListener('DOMContentLoaded', function (){
        const sliders = document.querySelectorAll(".slider");

        sliders.forEach((el) => {
          let input = el.querySelector("input");
          let value = el.querySelector("output");

          value.textContent = input.value;
          input.addEventListener("input", (event) => {
            value.textContent = event.target.value;
          })
        });
      })
    }

    # Sliders with displayed values

    You may want to display the value of the slider like so:

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

(define slider-js
  @html*{
    @script{
      document.addEventListener('DOMContentLoaded', function (){
        const sliders = document.querySelectorAll(".slider");

        sliders.forEach((el) => {
          let input = el.querySelector("input");
          let value = el.querySelector("output");

          value.textContent = input.value;
          input.addEventListener("input", (event) => {
            value.textContent = event.target.value;
          })
        });
      })
    }})

(define (macro-sliders)
  @md{
    @slider-js

    # Macro Sliders

    @form{
      submit-button
    }})

(define (end)
  @md{
    # The End

    If you want to restart this tutorial, click below.

    @button{Start from Beginning}})

(defstudy sliders
  [simple-slider --> working-with-selected-value
                 --> slider-display-value
                 --> macro-sliders
                 --> end
                 --> simple-slider])
