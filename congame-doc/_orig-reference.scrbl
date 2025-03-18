#lang scribble/manual

@(require (for-label congame/components/study
                     congame/components/transition-graph
                     (only-in forms form? widget-renderer/c)
                     racket/base
                     racket/contract
                     web-server/http
                     xml))

@title{Reference (Original)}

Moved elsewhere

@;|{ The content below is outdated and no longer belongs in the docs as far as I can tell.


@section{Data Storage & Retrieval}

A @deftech{step scope} represents the region of the database where
data for a study is stored and retrieved from.  Step scope is
determined by the combination of the current participant, the study
stack and optional round and group information.  @deftech{Instance
scope} is shared between participants to a study @tech{instance}.

@defproc[(get [k symbol?]
              [default (or/c any/c (-> any/c)) (λ () (error 'get "value not found for key ~.s" k))]
              [#:round round-name string? ""]
              [#:group group-name string? ""]) any/c]{

  Retrieves the value stored under the symbol @racket[k] for the
  current @tech{step scope}.  If no such value exists,
  @racket[default] is called if it is a procedure, or returned if it
  is a value.
}

@defproc[(get/instance [k symbol?]
                       [default (or/c any/c procedure?)]) any/c]{

  Like @racket[get], but retrieves data from @tech{instance scope}.
}

@defproc[(put [k symbol?]
              [v any/c]
              [#:round round-name string? ""]
              [#:group group-name string? ""]) void?]{

  Stores @racket[v] under the symbol @racket[k] for the current
  @tech{step scope}.
}

@defproc[(put/instance [k symbol?]
                       [v any/c]) void?]{

  Like @racket[put], but stores data in @tech{instance scope}.
}

@deftogether[(
  @defproc[(get-current-round-name) string?]
  @defproc[(put-current-round-name [round-name string?]) void?]
)]{
  Controls the current round for participants in a study.
}

@deftogether[(
  @defproc[(get-current-group-name) string?]
  @defproc[(put-current-group-name [group-name string?]) void?]
)]{
  Controls the current group for participants in a study.
}

}|
