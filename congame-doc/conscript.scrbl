#lang scribble/manual

@title{Conscript}

@section{Goals and Philosophy}

@deftech{conscript} aims to provide a simpler and streamlined syntax for writing studies compared to @tech{congame}. There are two dialects of @tech{conscript}:

@itemize[
  @item{Plain @tech{conscript}, which cannot require @tech{Racket} functions that we are not providing by default.}
  @item{@deftech{conscript/with-require}, which allows the user to require.}
]

Since @tech{conscript/with-require} provides a mechanism to require arbitrary Racket packages, it has essentially the same power as full @tech{congame}, while @tech{conscript} can only use a limited set of libraries and functionality of Racket. Due to this limited functionality of @tech{conscript}, we can allow such studies to be uploaded to a congame server by users with 'researcher' accounts, and be run without requiring access to the code repository. This is not possible (out of security concerns) with @tech{conscript/with-require} studies.

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

