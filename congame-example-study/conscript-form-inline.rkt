#lang conscript

(provide
 conscript-form-inline-example)

(defstep (start)
  @md{# A Form

      @form[#:fields
            ([a (input-text "field a")]
             [b (input-text "field b")])
            ]{@md*{Please answer the following questions:

                   @list*['div (shuffle (list a b))]
                   @input-text[#:c]{field c}

                   @submit-button}}})

(defstudy conscript-form-inline-example
  [start --> start])
