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
}

@study[
  randomized-list
  #:transitions
  [welcome --> display-randomized-list --> display-randomized-list]
]
