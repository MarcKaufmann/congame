#lang conscript

(provide
 conscript-markdown-example)

(defstep (start)
  @md{# Start

      This is the end of the study.

      @button{Continue}})

(defstep (script-example)
  @md{# Script Example

      @script{(() => {
                console.log("hello from the script");
              })()}

      @button{Back to start}})

(defstudy conscript-markdown-example
  [start --> script-example --> start])
