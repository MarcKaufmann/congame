@step[a]{
  a
  b

  c

  @h1{Heading}
}

@step[b]{
  a
  b

  @p{c}

  @h1{Heading}
}

@step[c]{
  a
  b

  @p{
    c

    d
  }

  @h1{Heading}
}

@step[d]{
  a
  b

  c

  @h1{Heading}

  d
  e

  f
}

@step[ordered-list]{
  @ol{
    @li{Test}
    @li{Foo}
  }
}

@step[unordered-list]{
  @ul{
    @li{Test}
    @li{Foo}
  }
}

@step[nested-lists]{
  @ul{
    @li{
      @ul{
        @li{Nested 1}
        @li{Nested 2}
      }
    }
    @li{Foo}
  }
}

@step[anchor]{
  @a["http://example.com"]{example.com}
}

@step[emphasis]{
  Hello @strong{world}!
}

@step[emphasis-nesting]{
  @strong{Hello @em{world}!}
}
