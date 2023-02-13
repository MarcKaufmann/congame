@import[racket/base format]
@action[test]{
  @(ev
    (begin
     (put 'counter1 3)
     (put 'counter2 (format "~a" "Hello"))
     (put 'sym 'test)))
}
