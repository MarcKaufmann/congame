#lang conscript

(provide
 tutorial-css
 reuse-css)

(defstep (basic-html)
  @html{
    @h1{A Page with Basic Formatting}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for @strong{strongly} emphasizing a word is @strong{strong}.

    @button{Continue}
})

(defstep (basic-md)
  @md{
    # A Page with Basic Formatting

    The syntax for *emphasizing* a word in markdown is to wrap a string in asterisks ("*"), while the syntax for **strongly** emphasizing a word is to wrap a string in double asterisks ("**").

    @button{Continue}
})

(defstep (inline-css)
  @html{
    @h1[#:style "color: red; text-align: center;"]{A Page with Basic Formatting}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for @strong{strongly} emphasizing a word is @strong{strong}.

    @button{Continue}
})

(defstep (messy-css)
  @html{
    @h1[#:style "color: red; text-align: center;"]{A Page with Multiple Repeated Inline Styles}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for @strong{strongly} emphasizing a word is @strong{strong}.

    @h2[#:style "color: red; text-align: center;"]{Another Header}

    @h2[#:style "color: red; text-align: center;"]{And Another}

    @button{Continue}
})

(defstep (clean-css)
  @html{

    @style{
      h1, h2 {
        color: red;
        text-align: center;
      }
    }

    @h1{A Page with a Style Element}

    The HTML tag for @em{emphasizing} a word is @em{em}, while the tag for @strong{strongly} emphasizing a word is @strong{strong}.

    @h2{Another Header}

    @h2{And Another}

    @button{Continue}
})


(defstudy tutorial-css
  [basic-html --> basic-md
              --> inline-css
              --> messy-css
              --> clean-css
              --> basic-html])

(define my-style
  @html*{
    @style{
      h1, h2 {
        color: red;
        text-align: center;
      }
    }
  })

(defstep (use-style)
  @html{

    @my-style

    @h1{First use}

    @button{Continue}
  })

(defstep (reuse-style)
  @html{

    @my-style

    @h1{Using the same style again}

  })

(defstudy reuse-css
  [use-style --> reuse-style
             --> reuse-style])
