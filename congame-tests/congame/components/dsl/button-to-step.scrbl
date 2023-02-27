@step[a]{
  @h1{Step a}
  @button[#:to-step c]{Next}
}

@step[b]{
  @h1{Step b}
  @button{Next}
}

@step[c]{
  @h1{Step c}
  @button{Next}
}

@study[
  s
  #:transitions
  [a --> b --> c]
  [c --> c]
]
