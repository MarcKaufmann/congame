@script{
  let num1 = 10;
  let num2 = 5;
  let sum = num1 + num2;
  alert(sum)
}

@step[first]{
  @h1{First}
  @button{Next}
}

@step[second]{
  @h1{Second}
  @button{Next}
}

@step[third]{
  @h1{Third}
}

@study[
  test-js
  #:transitions
  [first --> second --> third]
  [third --> third]
]
