#lang conscript

(provide
 conscript-example)

(defstep (info)
  @html{@h1{Hello!}
        Welcome to the study.
        @button{Continue}})

(defstep (consent)
  @html{@h1{Consent}
        Do you consent to join the study?
        @button{Give Consent}})

(defstep (final)
  @html{@h1{Done}
        You're done.

        @a[#:href (~current-view-uri)
           #:up-layer "new"
           #:up-target ".container"]{Overlay}})

(defview (done-overlay _req)
  @md{# Info

      Blah blah blah.})

(defstudy conscript-example
  [info --> {consent1 consent}
        --> {consent2 consent}
        --> {final (make-step
                   #:view-handler done-overlay
                   'final final)}]
  [final --> final])
