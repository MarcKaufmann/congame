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
  @h1{Randomized list}


  @ul{
    @(splicing-ev
      (for/list ([item (get 'items)])
        @li[(~a item)]))
  }
}

@study[
  randomized-list
  #:transitions
  [welcome --> display-randomized-list --> display-randomized-list]
]
