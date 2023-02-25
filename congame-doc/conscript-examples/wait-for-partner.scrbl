@action[assign-ids]{
  @(ev
     (begin
       @; 0 is partnered with 1; 2 with 3; ... 2*n with 2*n + 1
       (with-transaction
         (define next-id
           (get/instance 'next-id 0))
         @; FIXME: Could some of this code be moved out of the transaction, since it doesn't update instance? But how to share values?
         (put 'id next-id)
         (put 'partner-id
           (if (zero? (modulo next-id 2))
             (add1 next-id)
             (sub1 next-id)))
         (put/instance 'next-id (add1 next-id)))))
}

@step[welcome]{
  @h1{Welcome}

  @button[#:action assign-ids]{Next}
}

@action[share-answer-action]{
  @(ev
     (begin
       (define answer (get 'first-name))
       (define own-id (get 'id))
       (with-transaction
         (define answers (get/instance 'partner-answers (hash)))
         (put/instance 'partner-answers (hash-set answers own-id answer)))))
}

@step[give-answer]{
  @h1{Your answer}

  @form[#:action share-answer-action]{
    @input-text[first-name]{What is your first name?}
    @submit-button[]}
}

@action[check-partner]{
  @(ev
    (begin
      (define partner-id
        (get 'partner-id))
      (define partner-answer
        (hash-ref (get/instance 'partner-answers (hash)) partner-id #f))
      (when partner-answer
        (put 'partner-answer partner-answer)
        (skip))))
}

@step[wait #:pre check-partner]{
  @h1{Waiting}

  @refresh-every[5]
}

@step[display-answer]{
  @h1{Your partner's answer}

  Your partner is @(ev (get 'partner-answer)).
}

@study[
  wait-for-partner
  #:transitions
  [welcome --> give-answer --> wait --> display-answer]
  [display-answer --> display-answer]
]
