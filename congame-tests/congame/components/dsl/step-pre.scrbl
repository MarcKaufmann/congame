@action[pre-step-foo]{
  @(ev (put 'x 42))
}

@step[foo #:pre pre-step-foo]{
  Hello world
}
