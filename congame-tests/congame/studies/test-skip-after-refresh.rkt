#lang conscript

(require conscript/survey-tools)

(provide
 test-skip-after-refresh)

(defvar refreshed?)

(defstep (start)
  (set! refreshed? #f)
  @md{# Welcome

      @button{Continue...}})

(defstep (skip-after-refresh)
  (cond
    [(if-undefined refreshed? #f)
     (skip 'end)]
    [else
     (set! refreshed? #t)
     @md{# Please Wait

         @refresh-every[1]}]))

(defstep (end)
  @md{All done.})

(defstudy test-skip-after-refresh
  [start --> skip-after-refresh --> end --> ,(lambda () done)])
