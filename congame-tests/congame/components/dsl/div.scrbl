@step[div]{
  @div[#:id "bar"]{Hello @em{world}!}
}

@step[div-with-splitting]{
  @div[#:class "foo" #:style "font-weight: bold"]{
   Hello

   @em{world}!
  }
}

@step[div-with-class]{
  @div[#:class "example"]{
   Hello

   @em{world}!
  }
}
