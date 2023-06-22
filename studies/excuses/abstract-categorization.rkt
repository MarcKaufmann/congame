#lang racket/base

(require racket/generic
         racket/match
         racket/port
         racket/pretty
         racket/serialize
         web-server/http
         csv-reading
         koyo/haml
         congame/components/export
         congame/components/formular
         congame/components/study
         congame/components/transition-graph
         (submod congame/components/study accessors)
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
     (hash 'abstract text
           'categories categories
           'non-categories non-categories))])

(define *ABSTRACTS* #f)

(struct exn:fail:->abstract exn:fail ())

(define (exn:fail:csv-reader? e)
  (and (exn:fail? e)
       (regexp-match? #rx"%csv-reading" (exn-message e))))

(define (->abstract r)
  (if (not (= 3 (length r)))
      (raise (exn:fail:->abstract (format "row does not contain exactly three items: ~a" r) (current-continuation-marks)))
      (abstract (car r) (cadr r) (caddr r))))

(define (yn-radios label)
  (map-to-type
   (lambda (s) (string=? s "yes"))
   (radios label '(("yes" . "Yes")
                   ("no"  . "No")))))

; TODO: This call is ugly, change interface to string these together more conveniently. Pass in association list of lambdas and exceptions?
(define (input-abstracts label)
  (map-to-type/handler
   (lambda (rows)
     (map ->abstract rows))
   (map-to-type/handler
    (lambda (csv-file)
      (csv->list
       (binding:file/port-in csv-file)))
    (input-file label)
    #:the-exn? exn:fail:csv-reader?
    #:err-message (lambda (_e)
                    (format "error reading csv: we expect comma (,) not semicolon (;) as separator, and exactly 3 columns with text.~n You may also want to check for the BOM.)")))
   #:the-exn? exn:fail:->abstract?))

(define (upload-abstracts/page)
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
        (#:abstracts (input-abstracts "csv file with abstracts and categories"))
        (#:header? (yn-radios "Is the first row of the csv file a header row?"))
        submit-button))
      (lambda (#:abstracts abstracts
               #:header? header?)
        (put 'header? header?)
        (put/instance/top 'abstracts (if header? (cdr abstracts) abstracts))))))))

(define (display-abstracts)
  (haml
   (:div
    (:h3 "Abstracts")
    (:ul
     ,@(for/list ([a (get/instance/top 'abstracts)])
         (haml
          (:li (with-output-to-string
                 (lambda ()
                   (pretty-write (->jsexpr a)))))))))))


(define (check-abstracts)
  (define abstracts (get/instance/top 'abstracts))
  (page
   (haml
    (.container
     (:h1 "Check Abstracts")

     (:p "If you are fine with the abstracts, click 'Continue'. If you want to upload other abstracts, click 'Upload Abstracts' instead, which will overwrite the existing abstracts.")

     (button
      (lambda ()
        (set! *ABSTRACTS* abstracts))
      "Keep Abstracts")

     (button void "Upload other abstracts" #:to-step-id 'upload-abstracts)

     (display-abstracts)))))

(define (show-abstracts)
  (page
   (haml
    (.container
     (display-abstracts)))))

(define upload-abstracts
  (make-study
   "upload-abstracts-study"
   #:transitions
   (transition-graph
    [upload-abstracts --> check-abstracts
                      --> show-abstracts
                      --> show-abstracts])

   (list
    (make-step 'upload-abstracts upload-abstracts/page)
    (make-step 'check-abstracts check-abstracts)
    (make-step 'show-abstracts show-abstracts))))
