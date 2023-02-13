@step[step0]{@button{Continue}}
@step[step1]{@button{Continue}}
@step[step2]{@button{Continue}}
@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [step0 --> @(ev (lambda ()
                   (cond
                    [(= (get 'some-var) "agree") (goto step1)]
                    [(= (get/instance 'some-other-var) "always-agree") (goto step1)]
                    [else (goto step2)])))]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
