@define[start-moment (moment 2023 03 09 08)]
@define[task-date (today)]

@action[set-start-time]{
  @(ev
    (begin
      (put 'start-time (now/moment))))
}

@step[welcome]{
  @h1{Welcome}

  The current date is @(ev (~t (today) "yy-MM-dd")).

  The start date of this study is @(ev (~t start-moment "yy-MM-dd, HH:mm")).

  @button[#:action set-start-time]{Next}
}

@action[set-refresh-time]{
  @(ev
    (begin
      (put
        'refresh-time
        (+period (now/moment) (period [seconds 20])))))
}
@step[wait-then-click-to-move-on]{
  @h1{Please wait here}

  You have to wait 20 seconds until you can move on to the next page. Then you can click the button.

  @button[#:action set-refresh-time]{Continue}
}

@action[check-refresh-move-on]{
  @(ev
     (begin
       (when (moment>=? (now/moment) (get 'refresh-time))
         (skip))))
}

@step[wait-with-automatic-refresh-until-move-on
      #:pre check-refresh-move-on]{
  @h1{This page refreshes automatically}

  This page refreshes automatically and will move on automatically within the next minute.

  @refresh-every[5]
}

@action[check-task-date]{
  @(ev
    (begin
      (define t (today))
      (when (date=? t task-date)
        (skip))))
}

@step[cannot-do-the-task #:pre check-task-date]{
  @(ev
    (begin
      (define t (today))
      (cond [(date>? task-date t)
             @h1{You cannot yet do the task.}
             @p{Please come back later.}]
            [else
             @h1{You can no longer do the task.}])))
}

@step[the-task]{
  @h1{Do the task}

  Now do the task.

  @button{Next}
}

@step[the-end]{
  @h1{The End}
}

@study[
  dates-and-times
  #:transitions
  [welcome --> wait-then-click-to-move-on
           --> @(ev (lambda ()
                      (cond [(moment>=? (now/moment) (get 'start-time))
                             'wait-with-automatic-refresh-until-move-on]
                            [else
                             'wait-then-click-to-move-on])))]
  [wait-with-automatic-refresh-until-move-on --> cannot-do-the-task --> the-task --> the-end]
  [the-end --> the-end]
]
