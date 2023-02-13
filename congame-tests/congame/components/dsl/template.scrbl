@template-ungrouped[a]{
  Hello @yield[]!
}

@template[b]{
  @template[a]{
    Hi world!
  }

  @template[a]{
    Hello

    there

    @h1{Friend}
  }
}

@step[hello]{
  @template[b]
}
