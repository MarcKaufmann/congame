@import[racket/base println]

@action[quit]{
  @(ev (println "hello"))
}

@step[hello]{
  @button[#:action quit]{Quit}
}
