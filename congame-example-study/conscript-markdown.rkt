#lang conscript

(provide
 conscript-markdown-example)

(defstep (start)
  @md{# Start

      This is the end of the study.

      @button{Continue}})

(defstudy conscript-markdown-example
  [start --> start])
