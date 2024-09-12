#lang conscript

(require conscript/survey-tools)

(provide
 skip-after-refresh-study)

(defvar refreshed?)

(defstep (start)
  (set! refreshed? #f)
  @md{# Welcome

      @button{Continue...}})

(defstep (skip-after-refresh)
  (cond
    [(if-undefined refreshed? #f)
     (skip 'start)]
    [else
     (set! refreshed? #t)
     @md{# Please Wait

         @refresh-every[1]}]))

(defstep (end)
  @md{All done.})

(defstudy skip-after-refresh-study
  [start --> skip-after-refresh --> end --> ,(lambda () done)])
