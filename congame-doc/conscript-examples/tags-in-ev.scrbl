@step[number]{
  @(ev (cond [(even? 1) (h1 "Even")]
                 [else (h1 "Odd")]))

  @(ev (p "test " (strong "this") " bla "))

  @button{Next}
}

@step[get-two-numbers]{
  @h1{Get two numbers}

  @form[#:action create-ordered-list]{
    @input-number[a]{First number, called A.}
    @input-number[b]{Second number, called B.}
    @submit-button[]
  }

}

@action[create-ordered-list]{
  @(ev
    (begin
      (define unordered-list
        (for/list ([k '(a b)])
          (list k (get k))))
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
  [number --> get-two-numbers 
          --> display-ordered-items 
          --> display-ordered-items]
]
