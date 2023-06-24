#lang racket/base

(require racket/format
         racket/generic
         racket/match
         racket/port
         racket/pretty
         racket/random
         racket/serialize
         racket/string
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
 abstracts-config
 abstract-tasks)

; TODO: If we use this pattern a lot, we can define a macro to define-accessors
(define get/abstracts
  (make-get/top/root get #:root '*abstracts*))

(define put/abstracts
  (make-put/top/root put #:root '*abstracts*))

(define get/instance/abstracts
  (make-get/top/root get/instance #:root '*abstracts*))

(define put/instance/abstracts
  (make-put/top/root put/instance #:root '*abstracts*))

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
        (put/abstracts 'header? header?)
        (put/instance/abstracts 'abstracts (if header? (cdr abstracts) abstracts))))))))

(define (display-abstracts)
  (haml
   (:div
    (:h3 "Abstracts")
    (:ul
     ,@(for/list ([a (get/instance/abstracts 'abstracts)])
         (haml
          (:li (with-output-to-string
                 (lambda ()
                   (pretty-write (->jsexpr a)))))))))))


(define (check-abstracts)
  (define abstracts (get/instance/abstracts 'abstracts))
  (page
   (haml
    (.container
     (:h1 "Check Abstracts")

     (:p "If you are fine with the abstracts, click 'Continue'. If you want to upload other abstracts, click 'Upload Abstracts' instead, which will overwrite the existing abstracts.")

     (button
      (lambda ()
        (set! *ABSTRACTS* abstracts)
        (put/instance/top 'abstracts abstracts))
      "Keep Abstracts")

     (button void "Upload other abstracts" #:to-step-id 'upload-abstracts)

     (display-abstracts)))))

(define (show-abstracts)
  (page
   (haml
    (.container

     (button void "Finish Abstract Setup")

     (display-abstracts)))))

(define abstracts-config
  (make-study
   "configure abstracts"
   #:transitions
   (transition-graph
    [upload-abstracts --> check-abstracts
                      --> show-abstracts
                      --> ,(lambda () done)])

   (list
    (make-step 'upload-abstracts upload-abstracts/page)
    (make-step 'check-abstracts check-abstracts)
    (make-step 'show-abstracts show-abstracts))))

(define (abstract-task/page abs-task i total cat non-cat)
  (match-define (abstract a _category _non-category) abs-task)
  (page
   (haml
    (.container
     (:h2 (format "Categorize Abstract ~a out of ~a" i total))
     (:p (format "Decide whether this abstract is about ~a or not." cat))

     ; FIXME: use cat an non-cat to check if the answer is right.
     (:h4 "The Abstract")
     (:p a)
     (button void cat #:to-step-id 'categorize-in)
     (button void non-cat #:to-step-id 'categorize-out)))))

;; A sequence of abstract categorization tasks is defined by:
;; - a common category (a single category such as "Gender")
;; - a common non-category ("Other" by default)
;; - the list of abstracts to do
;; TODO: If we decide that failed abstracts need to be replaced, then we need to pass in the number of tasks to do as well as a list of abstracts sufficiently long to go through. For now, have a fixed number to categorize.

(define ((stub title [final? #f]))
  (page
   (haml
    (.container
     (:h1 title)
     (unless final?
       (haml
        (button void "Next")))))))

; It would be nice to be able to use syntactic sugar of the form
; (define-study (abstract-tasks* category non-category loa) ... instead of requires
(define (abstract-tasks*)

  (define get/loop
    (make-get/root get #:root '*loop*))
  (define put/loop
    (make-put/root put #:root '*loop*))

  (define (round-name i)
    (~a "abstract " i))

  (define (set-state! index abstracts)
    ; Assumes that `abstracts` is a non-empty list of abstracts
    ; FIXME: Add contract for this property.
    (set-current-round-name! (round-name index))
    (put/loop 'index index)
    (put/loop 'next-abstract (car abstracts))
    (put/loop 'remaining-abstracts (cdr abstracts)))

  (define (setup)
    (define loa (get 'abstracts))
    ; FIXME: assumes loa is non-empty list of abstracts. Ensure and/or check this somewhere.
    (define old-round-name (current-round-name))
    (put #:round "" 'old-round-name old-round-name)
    (put #:round "" 'total (length loa))
    (set-state! 1 loa)
    (skip))

; FIXME: syntactic sugar: (define (abstract-task @abstract @n @total) ...)
  (define (abstract-task)
    (define abs-task (get/loop 'next-abstract))
    (define i (get/loop 'index))
    (define total (get #:round "" 'total))
    (define category (get #:round "" 'category))
    (define non-category (get #:round "" 'non-category))
    (abstract-task/page abs-task i total category non-category))

  (define (loop)
    (define abstracts
      (get/loop 'remaining-abstracts))

    (cond [(null? abstracts)
           (set-current-round-name! (get #:round "" 'old-round-name))
           (skip)]

          [else
           (define new-index (add1 (get/loop 'index)))
           (set-state! new-index abstracts)
           (skip 'one-task)]))

  (make-study
   "sequence of n abstracts"
   #:requires '(abstracts category non-category)
   #:transitions
   (transition-graph
    [setup-loop --> one-task
                --> loop
                --> ,(lambda () done)]
    [categorize-in --> loop]
    [categorize-out --> loop])
   (list
    (make-step 'setup-loop setup)
    (make-step 'loop loop)
    (make-step 'one-task abstract-task)
    (make-step 'categorize-in (stub "Categorize in"))
    (make-step 'categorize-out (stub "Categorize out")))))

; FIXME: This assumes that cat and non-cat are a single category.
; We'll have to see if that holds.
(define (get-matching-abstracts loa n cat non-cat)
  (define (abstract-matches-cat a)
    (string-contains? (abstract-categories a) cat))
  (define (abstract-matches-non-cat a)
    (if (string=? "Other" non-cat)
        (string-contains? (abstract-non-categories a) cat)
        (string-contains? (abstract-categories a) non-cat)))

  (define all-matching-abstracts
    (filter
     (lambda (a)
       (let ([mc (abstract-matches-cat a)]
             [mnc (abstract-matches-non-cat a)])
         (and (or mc mnc)
              (not (and mc mnc)))))
     loa))

  (random-sample all-matching-abstracts n #:replacement? #f))

(define (initialize-abstracts)
  ; FIXME: The following is to scaffold for now
  (define n 2)
  (define category "Gender")
  (define non-category "Other")
  (put 'total-abstracts n)
  (put 'category category)
  (put 'non-category non-category)
  (put 'abstracts-to-do (get-matching-abstracts (get/instance/top 'abstracts) n category non-category))
  (skip))

(define (assigning-roles)
  (cond [(current-participant-owner?)
         (put/top 'role 'admin)
         (skip)]
        [else
         (put/top 'role 'participant)
         (skip)]))

(define (study-open?)
  (equal? (get/instance/top 'phase #f) 'open))

(define (waiting-page)
  (cond [(study-open?)
         (skip)]

        [else
         (page
          (haml
           (.container
            (:h1 "The study is not yet open")

            (:p "The study is not yet open for participants. Please come back later.")
            (:p "If you believe this is in error, please send an email to the study admin."))))]))

(define (admin)
  (page
   (haml
    (.container
     (:h1 "Admin")

     (unless (study-open?)
       (button void "Setup study" #:to-step-id 'admin-setup))))))

(define (switch-phase-to p #:check-current-phase [cp #f])
  (define old-phase (get/instance/top 'phase #f))
  (cond [(or (not cp) (and cp (equal? cp old-phase)))
         (put/instance/top 'phase p)]

        [else
         (error 'switch-phase-to "failed because the current phase is ~a, but needs to be ~a to switch phase" old-phase cp)]))

; TODO: Next this gets saved and the abstract tasks run. Check which of `category` and `non-category` still need to be passed in.
; Then create the tutorial tasks.
; Then create a single question where the person chooses, and use that as the input for the abstract tasks.
; Then create three questions where the person chooses, implement choice-that-matters, and use that to determine work.
(define abstract-tasks
  (make-study
   "abstract-tasks"
   #:transitions
   (transition-graph
    [assigning-roles --> ,(lambda ()
                            (let ([role (get 'role)])
                              (cond [(equal? role 'admin)       (goto admin)]
                                    [(equal? role 'participant) (goto waiting-page)]
                                    [else                       (goto error-page)])))]
    [error-page --> error-page]
    [admin --> admin]
    [admin-setup --> ,(lambda ()
                        (switch-phase-to 'open #:check-current-phase #f)
                        (goto admin))]
    [waiting-page --> initialize
                  --> tasks
                  --> thank-you
                  --> thank-you])
   (list
    (make-step 'assigning-roles assigning-roles)
    (make-step 'initialize initialize-abstracts)
    (make-step/study
     'tasks
     ; TODO: syntactic sugar where we can write
     ; (abstract-tasks* easy-abstracts "Gender" "Other") instead of
     ; passing #:require-bindings, with some notation to differentiate in-memory values from DB values.
     (abstract-tasks*)
     #:require-bindings `([abstracts    abstracts-to-do]
                          [category     (const "Gender")]
                          [non-category (const "Other")]))
    (make-step 'thank-you (stub "Thank you" #t))
    (make-step 'admin admin)
    (make-step/study 'admin-setup abstracts-config)
    (make-step 'waiting-page waiting-page)
    (make-step 'error-page (stub "Error Page")))))

(define easy-abstracts
  (list
   (abstract "First" "Gender" "Equality")
   (abstract "Second" "Gender" "Sport")
   (abstract "Third" "Sport" "Gender")
   (abstract "Fourth" "Sport" "Equality")))
