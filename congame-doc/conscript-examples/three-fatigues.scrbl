@step[description]{
  @h1{Fatigue Survey}

  In this study we will ask you three times how tired you are.

  @button{Start study}
}

@step[how-tired-are-you]{
  @h1{Fatigue question}

  @form{
    @input-number[fatigue #:min 1 #:max 5]{On a scale from 1 (very tired) to 5 (very awake), how tired are you?}
    @submit-button[]
  }
}

@step[done]{
  @h1{You are done!}

  Thank you for participating.
}

@study[
  fatigue
  #:transitions
  [how-tired-are-you --> @(ev (lambda () done))]
]

@study[
  three-fatigues
  #:transitions
  [description --> [fatigue1 fatigue]
               --> [fatigue2 fatigue]
               --> [fatigue3 fatigue]
               --> done]
  [done --> done]
]
