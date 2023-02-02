#lang scribble/manual

@title{Conscript}

@section{Grammar}

@racketgrammar*[
  #:literals (
    @; stmt
    action define import step study template template-ungrouped

    @; expr
    a br button div em escape form get h1 h2 h3 img li p put quote span strong ol ul yield

    @; form-expr
    checkbox input-date input-file input-number input-text input-time textarea submit-button

    @; transition
    --> cond
  )

  [stmt (action id action-expr ...+)
        (define id expr)
        (import module-id binding-id ...+)
        (step id expr ...+)
        (step id #:pre pre-action-id expr ...+)
        (study id #:transitions transition ...+)
        (template id stmt ...+)
        (template-ungrouped id stmt ...+)]

  [expr (a text expr)
        (br)
        (button maybe-action text ...+)
        call-expr
        (div maybe-option ... expr ...+)
        (em expr ...+)
        (escape s-expr)
        (form maybe-action form-expr ...+)
        get-expr
        (h1 expr ...+)
        (h2 expr ...+)
        (h3 expr ...+)
        (img string)
        (span maybe-class expr ...+)
        (strong expr ...+)
        (ol (li expr ...+) ...+)
        (ul (li expr ...+) ...+)
        (template id expr ...+)
        (yield)

        imported-id
        number
        string
        keyword
        (quote datum)]

  [call-expr (imported-id expr ...)]

  [get-expr (get expr)
            (get expr expr)
            (get #:instance expr)
            (get #:instance expr expr)]

  [put-expr (put id-expr expr)
            (put #:instance id-expr expr)]

  [form-expr (checkbox id expr ...+)
             (input-date id expr ...+)
             (input-file id expr ...+)
             (input-number id expr ...+)
             (input-text id expr ...+)
             (input-time id expr ...+)
             (input-number id #:min number expr ...+)
             (input-number id #:max number expr ...+)
             (input-number id #:min number #:max number expr ...+)
             (submit-button)
             (textarea id expr ...+)
             expr]

  [action-expr call-expr
               get-expr
               put-expr
               (escape s-expr)]

  [maybe-action (code:line)
                (code:line #:action action-id)]
  [maybe-option (code:line)
                (code:line #:class string)
                (code:line #:style string)]

  [transition [transition-entry ...+]]
  [transition-entry -->
                    target-id
                    cond-expr
                    [lambda action-expr ...+ end]
                    [lambda action-expr ...+ target-id]
                    [lambda action-expr ...+ cond-expr]]

  [target-id step-id
             study-id
             imported-id]
]

@section{Goals and Philosophy}

@deftech{conscript} is aimed at creating simple studies fast and without fuss. It provides a more user-friendly syntax than @tech{congame}, which requires taking a deep dive and using full @tech{Racket}. @tech{conscript} makes it especially convenient to mix the text seen by users with small amounts of logic. It is not well-suited to complicated logic, or studies that manipulate complicated data objects.

One nice feature of @tech{conscript} is the ability to compile it to full @tech{congame} studies, which can then be reused to build larger @tech{congame} studies --- although this feature should not be abused, since the code generated is not as readable as handcrafted code. Nothing beats artisanal code from your local code bakery.

For this reason, @tech{conscript} will never be feature-complete and expose the same power as @tech{congame}: doing so would necessarily lead to the same complexity. When @tech{conscript} proves too limited for your needs, it is time to take the red pill and take the deep dive into @tech{congame}.

