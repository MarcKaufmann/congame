#lang racket/base

(require racket/serialize
         koyo/haml
         congame/components/export
         congame/components/formular
         congame/components/study
         (submod congame/componeents/study accessors)
         (submod congame/components/formular tools))

;; How to the whole task work
;; - provide the abstracts in some form and, ideally, store in the DB
;; - access them easily
;; - display a single page when given the abstract and the topics
;; - find way of repeating a task
;;      - this should be generic: whether it is a task or some other step

;; They way to load the tasks is to create an admin page for the owner of the study to upload abstracts in a specific csv format
;; I need to figure out what types of
;; Shit, it's not enough to say what topic a given abstract falls into. We also need to highlight which topic they do *not* fit.
;; Is there a reason why we might want to categorize an article into one of several categories, instead of just A or not-A?

;; upload to DB on admin page of the study
;; - how to store in the DB? Serialize and deserialize as structs or hashes? Or json? It can't be in json, or rather, even if it is, it will be serialized anyway. That's what all put and get's do. If json is more efficient (yet fluid) than serializing, then we have to implement that kind of thing separately.
;; - So I can store it either as a hash, or as a serializable struct.

(provide
 upload-abstracts)

(serializable-struct abstract [text categories non-categories]
  #:transparent
  #:methods gen:jsexprable
  [(define (->jsexpr s)
     (match-define (abstract text categories non-categories) s)
     (hasheq 'abstract text
             'categories categories
             'non-categories non-categories))])

(define *ABSTRACTS* #f)

;; WIP: use Neil van dyke's csv-reading and be done with it. One more dependency, but so what. Life's too short.
(define (upload-abstracts)
  (page
   (haml
    (.container
     (:h1 "Upload a csv with the abstracts")
     (:p "The csv should contain the following columns (in that order):")
     (:ol
      (:li "Abstract Text")
      (:li "Categories that apply, separated by semicolons")
      (:li "Categories that definitely do not apply, separated by semicolons"))

     (formular
      (haml
       (:div
        (#:abstracts-csv (input-file "csv with abstracts and categories"))
        submit-button))
      (lambda (#:abstracts-csv abstracts-csv)
        (define upload
          (upload-file! abstracts-csv))
        (put/top 'abstracts upload #:root '*abstracts*)
        (define abstracts
          (call-with-input-file abstracts-csv
            (lambda (in)
              (for/list ([line (in-lines in)])
                (match-define (list abst wl))))))

        ))))))
