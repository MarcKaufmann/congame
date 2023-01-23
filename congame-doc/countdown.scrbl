@; import some helper functions
@import[stdlib format sub1]

@action[initialize-counter]{
  @; This stores the number 10 in the value of counter
  @put['counter 10]
}

@action[decrement-counter]{
  @; This will overwrite the previous value of 'counter for this person
  @put['counter @call[sub1 @get['counter]]]
}

@step[initialize]{
  @h1{Initializing counter}

  @button[#:action initialize-counter]{Initialize the Counter!!}
}

@step[display-counter]{
  @h1{Counter is @call[format "~a" @get['counter]]}

  @button[#:action decrement-counter]{Count down!}
}

@study[
  countdown
  #:transitions
  [initialize --> display-counter --> display-counter]
]
