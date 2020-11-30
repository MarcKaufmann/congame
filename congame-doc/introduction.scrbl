#lang scribble/manual

@(require (for-label congame/components/study
                     racket/contract
                     web-server/http))

@title{Introduction}

@defproc[(run-study [s study?]
                    [req request? (current-request)]
                    [#:bindings bindings (hash/c symbol? any/c) (hasheq)]) any/c]{

  Runs the study @racket[s] under @racket[req] with @racket[bindings].
}
