@script{
  console.log("Running last!")
}

@style{
  h1 {
    text-decoration: underline;
  }
}

@step[example]{
  @h1{Classy and stylish images}

  @img["/static/img/play.png"]

  @h2{Yellow border with Style}

  @img[#:style "border: solid yellow 10px;" "/static/img/play.png"]

  @h2{Yellow border with Class}

  @style{
    .yellow-border { border: solid yellow 10px; }
  }

  @img[#:class "yellow-border" "/static/img/play.png"]

  @h2{Let's make all images smaller}

  @style{
    img {
      width: 30px;
    }

    h2 {
      color: red;
      background-color: purple;
    }
  }

  @h2{Let's log our success!}

  @script{
    console.log("Success! We got us some yellow borders!")
  }

}

@study[
  images
  #:transitions
  [example --> example]
]
