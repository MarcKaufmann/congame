#lang conscript

;; The parent and the substudy are communicating via a namespaced
;; global var. The parent increments the task number and the child
;; displays it.

(provide
 conscript-communication)

(defstep (start)
  (define (action)
    (set! task-n (add1 (if-undefined task-n 0))))
  @md{Welcome to the study

      @button[action]{Continue}})

(with-namespace xyz.trichotomy.conscript.conscript-communication
  (defvar* task-n))

(defstep (task)
  @md{Task number @~a[task-n]

      @button{Continue}})

(defstudy task-study
  [task --> ,(λ () done)])

(defstudy conscript-communication
  [start --> task-study --> start])
