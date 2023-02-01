@import[stdlib number->string sub1]

@action[initialize-counter]{
  @put['counter 10]
}

@action[decrement-counter]{
  @put['counter @call[sub1 @get['counter]]]
}

@step[description]{
  @h1{The countdown is about to begin}

  @button[#:action initialize-counter]{Start Countdown}
}

@step[show-counter]{
  @h1{@call[number->string @get['counter]]}

  @button[#:action decrement-counter]{Count down!}
}

@step[launch]{
  @div[#:style "color: red; font-size: 2rem;"]{
    @h1{Study launched!}
  }
}

@study[
  countdown
  #:transitions
  [description --> show-counter
               --> @cond[[@=[@get['counter] 0]
                         launch]
                        [@else
                         show-counter]]]
  [launch --> launch]
]
