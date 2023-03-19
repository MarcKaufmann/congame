@action[set-refresh-time]{
  @(ev
    (begin
      (put/moment
        'refresh-time
        (+period (now/moment) (period [seconds 5])))))
}


@action[check-refresh-move-on]{
  @(ev
     (begin
       (when (moment>=? (now/moment) (get/moment 'refresh-time))
         (skip))))
}

@step[first-set-refresh-time]{
  Hi
  @button[#:action set-refresh-time]{Start}
}


@step[wait-with-automatic-refresh-until-move-on #:pre check-refresh-move-on]{
  @h1{This page refreshes automatically}

  This page refreshes automatically and will move on automatically within the next 5 Seconds.

  @refresh-every[5]
}

@step[ending]{
  done
}


@study[
  date-time-with-wait-time
  #:transitions
  [first-set-refresh-time --> wait-with-automatic-refresh-until-move-on --> ending --> ending]
]
