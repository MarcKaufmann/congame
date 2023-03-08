@define[items '(0 1 2 3)]

@action[randomize-list]{
  @(ev
    (begin
      (put 'items (shuffle items))))
}

@step[welcome]{
  @h1{Welcome}

  @button[#:action randomize-list]{Next}
}

@step[display-randomized-list]{
  @h1{Randomized list and its sum}


  @ul{
    @(splicing-ev
      (for/list ([item (get 'items)])
        @li[(~a item)]))
    @(ev
      (~a (apply + (get 'items))))
  }

  @button{Next}
}

@step[display-randomized-forms]{
  @h1{Randomized form}

  @form{
    @input-text[text1]{The first question: @(ev (~a (first (get 'items))))}
    @input-text[text2]{The second question: @(ev (~a (second (get 'items))))}
    @input-text[text3]{The third question: @(ev (~a (third (get 'items))))}
    @input-text[text4]{The fourth question: @(ev (~a (fourth (get 'items))))}
    @submit-button[]
  }
}

@step[the-end]{
  @h1{The End}
}

@study[
  randomized-list
  #:transitions
  [welcome --> display-randomized-list
           --> display-randomized-forms
           --> the-end]
  [the-end --> the-end]
]
