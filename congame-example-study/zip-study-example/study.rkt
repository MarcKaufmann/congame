#lang conscript

(provide
 image-example)

(define-static-resource images "img")

(defstep (show-image)
  @html{
    @h1{A Page with an Image}

    @img[
      #:alt "Screenshot of Code"
      #:src (resource-uri images "code-screenshot.png")
    ]})

(defstudy image-example
  [show-image --> show-image])
