@action[initialize-counter]{
  @(ev (put 'counter 10))
}

@action[decrement-counter]{
  @(ev (put 'counter (sub1 (get 'counter))))
}

@step[description]{
  @h1{The countdown is about to begin}

  @button[#:action initialize-counter]{Start Countdown}
}

@step[show-counter]{
  @h1{@(ev (~a (get 'counter)))}

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
               --> @(ev (lambda ()
                         (cond
                          [(= (get 'counter) 0) 'launch]
                          [else 'show-counter])))]  @; need goto support here and also in transition-graph impl such that it can walk down interpret-basic-expr ... exprs
  [launch --> launch]
]
