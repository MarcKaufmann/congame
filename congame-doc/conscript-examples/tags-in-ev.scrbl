@step[number]{
  @(ev (cond [(even? 1) (h1 "Even")]
                 [else (h1 "Odd")]))

  @(ev (p "test " (strong "this") " bla "))

  @button{Next}
}

@step[get-some-numbers]{
  @h1{Get some numbers}

  @form[#:action create-ordered-list]{
    @input-number[a]{First number, called A.}
    @input-number[b]{Second number, called B.}
    @input-number[c]{Second number, called C.}
    @input-number[d]{Second number, called D.}
    @submit-button[]
  }

}

@action[create-ordered-list]{
  @(ev
    (begin
      (define unordered-list
        (for/list ([k '(a b c d)])
          (list k (get k))))
      @; => '( (a 1) (b 2) (c 3) (d 4))
      (define ordered-list
        (sort unordered-list (lambda (x y) (> (second x) (second y)))))
      (put 'ordered-list ordered-list)))
}

@step[display-ordered-items]{
  @(splicing-ev
    (for/list ([item (get 'ordered-list)])
      @p[(format "Item named ~a had value ~a" (first item) (second item))]))
}

@study[
  tags-in-ev
  #:transitions
  [number --> get-some-numbers
          --> display-ordered-items 
          --> display-ordered-items]
]
