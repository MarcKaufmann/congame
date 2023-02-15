@step[prep]{
  @h1{Do we need a first step that is not an action?}

  @button{Next}
}

@action[assign-roles]{
  @(ev
    (begin
      (define next-pid
        @; (get 'key value-if-key-not-found) returns the value of 'key,
        @; and the value-if-key-not-found if the key is not found
        (get/instance 'pid 0))
      (put 'pid next-pid)
      (cond [(even? next-pid) (put 'role "Dictator")]
            [else (put 'role "(Non-)Receiver")])
      (put/instance 'pid (add1 next-pid))
    ))
}

@step[start #:pre assign-roles]{
  @h1{You are in the role of @(ev (get 'role))}

  @button{Next}
}

@step[choice]{

  @form{
    @radios[
      payment
      '(
        (10 . "$10 for yourself, $0 for the other perons")
        (5  . "$5 for yourself, $5 for the other person")
       )
    ]{Please choose which of these options you prefer:}
    @submit-button[]}
}

@action[check-answer]{
  @(ev
    (begin
      @; We automatically pair participant with id 1 with participant
      @; with id 0; participant with id 3 with participant with id 2;
      @; and so on
      (define payments
        (get/instance 'payments (hash)))
      (define receiver-payment
        (hash-ref payments (get 'pid) #f))
      (cond [receiver-payment (put 'payment receiver-payment)]
            [else (put 'payment #f)])))
}

@step[wait]{

  @h1{Refresh this screen regularly}

  Check back later to see if your partner has made their choice yet.

  @button[#:action check-answer]{Check the answer}
}

@action[update-receiver-payment]{
  @(ev
     (begin
       (define receiver-id
         (add1 (get 'pid)))
       (define receiver-payment
         (- 10 (get 'payment)))
       (define current-payments
         (get/instance 'payments (hash)))
       (define new-payments
         (hash-set current-payments receiver-id receiver-payment))
       (put/instance 'payments new-payments)))
}

@step[display-dictator #:pre update-receiver-payment]{
  @h1{You will receive $@(ev (get 'payment))}
}

@step[display-receiver]{
  @h1{You will receive $@(ev (get 'payment))}
}

@study[
  baby-dictator
  #:transitions
  @; everyone
  [prep --> start --> @(ev (cond [(even? (get 'pid)) 'wait]
                        [else choice]))]
  @; dictator
  [choice --> display-dictator]
  [display-dictator --> display-dictator]
  @; receiver
  [wait --> @(ev (cond [(get 'payment #f) display-receiver]
                       [else wait]))]
  [display-receiver --> display-receiver]
]
