#lang scribble/manual

@(require [for-label conscript/base
                     (except-in conscript/survey-tools make-sliders)
                     racket/contract]
          "doc-util.rkt")

@title{Conscript Cookbook}

What follows are a bunch of “recipes” or how-tos that explain how to accomplish common tasks when
writing Conscript studies.

These how-tos don’t directly explain everything, but they do give links to more information,
including the links in the code samples themselves.

@; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section{How to add links}

To provide a link on a study page, use the anchor tag @racket[a]:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript

(defstep (links)
  @md{# Links

      A link to the @a[#:href "https://example.com/"]{another site}.

      Sometimes you want a link to open in a new tab, so you provide the
      attribute `target` with the value `"_blank"`:

      @a[#:href "https://example.com" #:target "_blank"]{Open in new tab}
  }) 
}|

@; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@section{How to display monetary amounts}

To display monetary amounts, first @racket[require] the module
@racketmodname[conscript/survey-tools] which provides @racket[~$] for dollars, @racket[~euro] for
euros, or @racket[~pound] for pounds:

@codeblock[#:keep-lang-line? #f]|{
#lang conscript
(require conscript/survey-tools)

(define (payments)
  (define bonus 4.25)
  @md{# Bonus

      Your bonus is @(~$ bonus).
  })
}|
