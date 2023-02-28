@action[assigning-treatments]{
  @(ev
    (begin
      (define treatments (list 'control 'treatment))
      (assigning-treatments treatments)))
}

@step[welcome]{
  @h1{Welcome}

  @button[#:action assigning-treatments]{Next}
}

@step[show-treatment]{
  @h1{Your treatment is @(ev (~a (get/global 'role)))}

  @button{Next}
}

@study[
  child-study
  #:transitions
  [show-treatment --> @(ev (lambda () done))]
]

@study[
  assign-treatments
  #:transitions
  [welcome --> child-study --> show-treatment]
  [show-treatment --> show-treatment]
]
