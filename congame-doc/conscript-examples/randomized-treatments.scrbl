@; We want to randomize every group of 10 participants into three treatments:
@; - 2 buyers
@; - 3 sellers
@; - 5 observers
@; We will do this as follows: whenever we have randomized a full set of participants (or at the very start), we set the list of treatments on the instance to the full, set: '(buyer buyer seller seller seller observer observer observer observer observer) and randomize (with `shuffle`) the order of these treatments. For every participants that arrives, we assign them the first role in this list and remove that role from the remaining list of treatments.

@; After assigning them to treatments, we display the role to the participant, and then we start each of them off to pursue their own treatments.

@; action for assigning treatments, run after the welcome screen
@action[assigning-treatments]{
  @(ev
    (begin
      (define treatments
        (list
          'buyer 'buyer 'seller 'seller 'seller
          'observer 'observer 'observer 'observer 'observer))
      (unless (get 'role #f) @; ensures we run this action only once
        @; run actions inside transaction to avoid multiple participants
        @; attempting to change instance variables at the same time
        (with-transaction
          @; first ensure that we have treatments defined
          (when (empty? (get/instance 'treatments '()))
            @; This means that we start a new round
            (put/instance 'treatments (shuffle treatments)))
          (define remaining-treatments
            (get/instance 'treatments))
          (define role
            @; (first l) gets the first item in the list
            (first remaining-treatments))
          @; save this as the role of the participant
          (put 'role role)
          @; (rest l) gets the list l without the first item
          (define updated-treatments
            (rest remaining-treatments))
          @; we have to update the value of treatments in the DB (database)
          (put/instance 'treatments updated-treatments)))))
}

@step[welcome]{
  @h1{Welcome screen}

  @button[#:action assigning-treatments]{Next}
}

@step[display-treatments]{
  @h1{Display Treatments}

  Your role is @(ev (symbol->string (get 'role))).

  @button{Next}
}

@step[thank-you]{
  @h1{Thank you!}
}

@; OBSERVER
@step[observer]{
  @h1{Observer step}

  Only observing, don't get to do anything.

  @button{Next}
}

@; SELLER
@step[seller-choice]{
  @h1{Seller Choice}

  @button{Next}
}

@step[show-seller-choice]{
  @h1{Show seller choice}

  @button{Next}
}

@study[
  seller
  #:transitions
  [seller-choice --> show-seller-choice --> (ev (lambda () done))]
]

@; BUYER
@step[buyer-choice]{
  @h1{Buyer choice}

  @button{Next}
}

@step[show-buyer-choice]{
  @h1{Show buyer choice}

  @button{Next}
}

@study[
  buyer
  #:transitions
  [buyer-choice --> show-buyer-choice --> (ev (lambda () done))]
]

@; COMPLETE STUDY
@study[
  assign-treatments
  #:transitions
  [welcome --> display-treatments --> @(ev (lambda ()
                                             (cond [(equal? 'observer (get 'role)) 'observer]
                                                   [(equal? 'seller (get 'role))    'seller]
                                                   [(equal? 'buyer (get 'role))     'buyer]
                                                   [else (error "treatment-randomization: expected to have a role of 'observer, 'buyer, or 'seller, but instead found " (get 'role))])))]

  @; separate studies for the different treatments
  [observer --> thank-you]
  [seller --> thank-you]
  [buyer --> thank-you]
  [thank-you --> thank-you]
]
