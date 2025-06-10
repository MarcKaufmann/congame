#lang scribble/manual

@(require (for-label racket/base
                     conscript/form)
          "doc-util.rkt")

@title[]{Legacy (Deprecated) Modules}

@;===============================================

@section{Forms}

@defmodule[conscript/form]

This module is deprecated in favor of the new approach in @racketmodname[conscript/form0].

@mark{Despite the above, the bindings provided by this module are also provided by @racketmodname[conscript/base].}

In addition to the bindings documented here, this module also reprovides most of the bindings in
@racketmodname[congame/components/formular].

@defproc[(bot:autofill [arg any/c]) any/c]{

@tktk{bot:autofill proc}

}


@defproc[(radios [arg any/c]) any/c]{

radios proc

}

@defproc[(select [arg any/c]) any/c]{

select proc

}

@defform[(binding arg)
         #:contracts ([arg any/c])]{

binding form

}

@defform[(form arg)
         #:contracts ([arg any/c])]{

form form

}