#lang scribble/manual

@(require (for-label racket
                     (except-in forms form)
                     koyo/haml
                     congame/components/study
                     congame/components/formular))

@title{Simple forms with Formular}

@defmodule[congame/components/formular]

@racket[formular] provides a simpler way to generate basic forms than @racket[forms], but it is not powerful enough to create more complicated forms. Moreover, while it is possible to use forms in one page and formular in another, it is not possible to compose the two on the same page.

The following is an example of a complete form used within a page:

@racketblock[
(page
  (haml
    (.container
      (formular
        (haml
          (:div
            (#:age (input-number "How old are you?")))
          (:div
            (#:name (input-text "What is your name?"))
          (:div
            (#:opinion (input-textarea "What is your opinion about this form?"))))
          (:button.button.next-button ([:type "submit"]) "Submit"))
        (λ (#:age age
            #:name name
            #:opinion opinion)
          (put 'age age)
          (put 'name name)
          (put 'opinion opinion))))))
]

@defform[(formular formular-expr)]{
  Creates a form widget.
}

@defproc[(input-text [label string?]
                     [#:validators validators list? null])
         (-> [meth symbol?] any/c)]{
  Creates a formular widget for simple text input.
}

@defproc[(input-textarea [label string?]
                         [#:validators validators list? null])
         (-> [meth symbol?] any/c)]{
  Creates a formular widget for a textarea input field.
}

@defproc[(input-number [label string?]
                       [#:min min real? -inf.0]
                       [#:max max real? +inf.0]
                       [#:validators validators list? null])
         (-> [meth (or 'validator 'widget)] any/c)]{
  Creates a formular widget for a number field. @racket{min} and @racket{max} set the range of allowed numbers.
  @; Does @racket{input-number} only allow integers right now, or is that merely the natural step size, but it allows arbitrary numbers?
}
