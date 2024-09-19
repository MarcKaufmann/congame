#lang scribble/manual

@(require (for-label racket/base
                     racket/contract))

@title[#:style 'toc]{Congame}
@author[(author+email "Marc Kaufmann" "marc@trichotomy.xyz")]

Congame is a platform for designing and deploying web-based research studies, and is @emph{the best
experimental economics software written in Racket}. 

Once deployed, a Congame @tech{study} becomes a web application. Using their browsers, people can
enroll in the study and participate by walking through each of the study’s steps, providing
responses to questions and even interacting in controlled ways with other participants. Congame
collects the responses and provides the data to each study author for further analysis.

Congame includes:

@itemlist[
    @item{Server applications and libraries for hosting @tech{studies} (the main @racketmodname[congame]
    collection)}

    @item{@tech{Conscript}, a scripting environment for prototyping studies.
    Using Conscript, you can develop studies rapidly on your local computer, without having to stand
    up a complete Congame server environment. When ready, a Conscript study can be uploaded to a Congame
    server.}
]

@table-of-contents[]

@include-section["introduction.scrbl"]
@include-section["concepts.scrbl"]
@include-section["conscript-section.scrbl"]
@include-section["congame-section.scrbl"]
@include-section["_orig-congame.scrbl"]