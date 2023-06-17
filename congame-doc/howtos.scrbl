#lang scribble/manual

@(require "howtos.rkt"
          (for-label congame/components/study
                     racket/base
                     racket/contract))

@title{How Tos}
@section{How to add a new study}

The simplest case is when you add a new study from a package that is
not yet installed. In that case, define the new study and provide it:
say the file that provides it is @filepath{package/new-studies.rkt}
and the name of the new study is @racket[test-study]. Then in the
@filepath{info.rkt} file for the package, include the following:

@racketblock[
(define congame-studies
  '((the-package/new-studies test-study)))
]

You then should install this new package, and upon the next launch,
congame should pick up this new study.

If this does not work, you may have to remove the @filepath{compiled} folder in
@filepath{congame-web/studies/} so that the cache (which contains the installed
studies) gets refreshed.

To add a new study from an already installed package, you should
update the @filepath{info.rkt} file of the package, recompile the package via @exec{raco setup <package-name>}, and then refresh the cache.

@section{How to pass values from a substudy to its caller}

When running a study with @racket[make-step/study], then the values
@racket[put] can not be retrieved anywhere with @racket[get] except in
this substudy. To pass some variables up to the calling study, we can
use the @racket[#:provides] mechanism. For example, consider the
following block of code from @filepath{congame-example-study/multi-review}:

@racketblock[
(define (submit-research-ideas [n 2])
  (code:comment "...")
  (make-study
   "research-ideas-study"
   #:requires '()
   #:provides '(research-ideas)
   (list
    (make-step 'initialize initialize next-or-done/transition)
    (make-step 'submit-research-idea submit-research-idea next-or-done/transition))))

(define (review-study)
  (make-study
   "review-study"
   #:requires '()
   #:provides '()
   (list
    (make-step/study
     'submit
     (submit-research-ideas)
     #:provide-bindings '([submission research-ideas]))
    (make-step 'update-submissions update-submissions)
    (make-step 'lobby lobby)
    (make-step 'review-1 review)
    (make-step 'review-2 review)
    (make-step 'final final))))
]

The study @racket[submit-research-ideas] @racket[put]s a value with
the key @racket['research-ideas], which it provides via
@racket[#:provides '(research-ideas)]. This means that this value can
be used by the parent study, if so desired. The parent study turns the
study into a substudy via @racket[(make-step/study 'submit
submit-research-ideas) #:provide-bindings '([submission
research-ideas])]. The magic happens in @racket[#:provide-bindings
'([submissions research-ideas])], which means that the value of key
@racket['research-ideas] for the substudy should be made available
(via @racket[put]) to the parent study under the key
@racket['submission]. If the parent study did not want to use the
provided value, then it can do so by providing an empty list of
@racket[#:provide-bindings].


@section{Scheduling tasks at most once}

See @filepath{omnitrack/track-physical.rkt} for an example.

Within a step and before the @racket[page] form is used, compute a
unique random value -- the @deftech{nonce} -- store it in the
@tech{step scope}, and call your desired task with that @tech{nonce}
and the participant id as arguments.  When the task runs, it should
look up the @tech{nonce} from @tech{step scope} and, if it matches,
perform its duties and then remove the @tech{nonce}.  Otherwise, it
should do nothing.


@section{Style Guide}

@compare0[
  @codeblock0[#:keep-lang-line? #f]|{
    #lang scribble/manual
    @; don't indent top-level forms
    @step[my-step]{
      @; within a form, ident using two spaces
      @h1{Hello World}
    }
  }|
  @codeblock0[#:keep-lang-line? #f]|{
    #lang scribble/manual

      @step[my-step]{

      @h1{Hello World}
      }
  }|
]
