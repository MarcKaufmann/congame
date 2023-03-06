@action[assign-roles]{
  @(ev
    (begin
      (with-transaction
       (unless (get 'role #f)
        (define next-pid
          (get/instance 'pid 0))
        (put 'pid next-pid)
        (cond [(even? next-pid) (put 'role "Dictator")]
              [else (put 'role "(Non-)Receiver")])
        (put/instance 'pid (add1 next-pid))))))
}

@step[start #:pre assign-roles]{
  @h1{You are in the role of @(ev (get 'role))}

  @ul{
    @(splicing-ev
      (for/list ([x '(1 2 3 4)])
        @li[(~a (add1 x))]))
  }

  @button{Next}
}

@step[choice]{

  @form{
    @radios[
      payment-string
      '(
        ("10" . "$10 for yourself, $0 for the other person")
        ("5"  . "$5 for yourself, $5 for the other person")
       )
    ]{Please choose which of these options you prefer:}
    @submit-button[]}
}

@action[check-on-dictator]{
  @(ev
    (begin
      @; We automatically pair participant with id 1 with participant
      @; with id 0; participant with id 3 with participant with id 2;
      @; and so on
      (define payments
        (get/instance 'payments (hash)))
      (define receiver-payment
        (hash-ref payments (get 'pid) #f))
      (when receiver-payment
        (put 'payment receiver-payment)
        (skip))))
}

@step[wait #:pre check-on-dictator]{
  @h1{Waiting...}

  Waiting for the dictator to make a choice.

  @refresh-every[5]
}

@action[update-receiver-payment]{
  @(ev
     (begin
       (define payment
         (string->number (get 'payment-string)))
       (put 'payment payment)
       (define receiver-id
         (add1 (get 'pid)))
       (define receiver-payment
         (- 10 payment))
       (define current-payments
         (get/instance 'payments (hash)))
       (define new-payments
         (hash-set current-payments receiver-id receiver-payment))
       (put/instance 'payments new-payments)))
}

@step[display-dictator #:pre update-receiver-payment]{
  @h1{You will receive $@(ev (number->string (get 'payment)))}
}

@step[display-receiver]{
  @h1{You will receive $@(ev (number->string (get 'payment)))}
}

@study[
  baby-dictator
  #:transitions
  @; everyone
  [start --> @(ev (lambda () (cond [(even? (get 'pid)) 'choice]
                        [else 'wait])))]
  @; dictator
  [choice --> display-dictator]
  [display-dictator --> display-dictator]
  @; receiver
  [wait --> display-receiver]
  [display-receiver --> display-receiver]
]
