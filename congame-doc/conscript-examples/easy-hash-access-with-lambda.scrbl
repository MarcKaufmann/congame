@define[h (hash 'a (hash 'a 1)
                'b (hash 'b 2))]

@define[easy-get (lambda (key)
                   (hash-ref (hash-ref h key) key))]

@step[display]{
  @h1{Show contents of hash}

  @ul{
    @li{For key 'a the value is: @(ev (~a (easy-get 'a)))}
    @li{For key 'b the value is: @(ev (~a (easy-get 'b)))}}
}

@study[
  lambda-for-hash
  #:transitions
  [display --> display]
]
