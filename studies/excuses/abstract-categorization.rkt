#lang at-exp racket/base

(require csv-reading
         koyo/haml
         racket/format
         racket/generic
         racket/list
         racket/match
         racket/math
         racket/port
         racket/pretty
         racket/random
         racket/serialize
         racket/string
         threading
         web-server/http
         congame/components/export
         congame/components/for-study
         congame/components/formular
         congame/components/study
         congame/components/transition-graph
         (submod congame/components/study accessors)
         (submod congame/components/formular tools))

(provide
 abstracts-admin
 abstract-tasks
 get-topics-stats
 random-abstract-matching
 task-description)

; TODO: If we use this pattern a lot, we can define a macro to define-accessors
(define get/abstracts*
  (make-get/root get* '*abstracts*))

(define put/abstracts*
  (make-put/root put* '*abstracts*))

(define get/instance/abstracts*
  (make-get/root get/instance* '*abstracts*))

(define put/instance/abstracts*
  (make-put/root put/instance* '*abstracts*))

(define (get-topics-stats)
  (define (n-topics h t)
    (length (hash-ref h t '())))
  (define topics (get/instance/abstracts* 'topics))
  (define non-topics (get/instance/abstracts* 'non-topics))
  (define all-topic-labels (remove-duplicates (sort (append (hash-keys topics) (hash-keys non-topics)) string<?)))
  (for/list ([t all-topic-labels])
    (list t (n-topics topics t) (n-topics non-topics t))))

(serializable-struct abstract [text categories non-categories]
  #:transparent
  #:methods gen:jsexprable
  [(define (->jsexpr s)
     (match-define (abstract text categories non-categories) s)
     (hash 'abstract text
           'categories categories
           'non-categories non-categories))])

(struct exn:fail:->abstract exn:fail ())

(define (exn:fail:csv-reader? e)
  (and (exn:fail? e)
       (regexp-match? #rx"%csv-reading" (exn-message e))))

(define (->abstract r)
  (if (not (= 3 (length r)))
      (raise (exn:fail:->abstract (format "row does not contain exactly three items: ~a" r) (current-continuation-marks)))
      (abstract (car r) (cadr r) (caddr r))))

(define (yn-radios label)
  (cast-result
   (radios label '(("yes" . "Yes")
                   ("no"  . "No")))
   (lambda (s) (string=? s "yes"))))

(define (abstract-example example)
  (match-define (list id text cat) example)
  (haml
   (:div
    (:h3 "Example")

    (:p "Consider the following abstract:")

    (:p text)

    (:p (format "It belongs in '~a'." cat)))))

(define (task-description n-tasks example)
  (page
   (haml
    (.container
     @:h1{Task Description}

     @:p{As mentioned before, the task you have to do in this study consists of categorizing abstracts (short summaries of research papers) by topics.}

     @(abstract-example example)

     @:h3{Your Turn}

     @:p{Now it is your turn to do @(~a n-tasks) tasks: continue to the next page to categorize @(~a n-tasks) abstracts.}

     @button[void]{Continue}))))

; TODO: This call is ugly, change interface to string these together more conveniently. Pass in association list of lambdas and exceptions?
(define (input-abstracts label)
  (~> (input-file label)
      (cast-result*
       (compose1 (lambda (f) (csv->list (make-csv-reader f '((separator-chars #\;))))) binding:file/port-in)
       #:exn-predicate exn:fail:csv-reader?
       #:exn-handler (λ (_e) "error reading csv: we expect comma (,) not semicolon (;) as separator, and exactly 3 columns with text.~n You may also want to check for the BOM.)"))
      (cast-result*
       #:exn-predicate exn:fail:->abstract?
       (λ (rows)
         (map ->abstract rows)))))

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
        (put/abstracts* 'header? header?)
        (put/instance/abstracts* 'raw-abstracts (if header? (cdr abstracts) abstracts))))))))

(define (display-abstracts)
  (haml
   (:div
    (:h3 "Abstracts")
    (:table
     (:thead
      (:tr
       (:th "ID") (:th "Abstract")))
     (:tbody
     ,@(for/list ([(i a) (get/instance/abstracts* 'abstracts)])
         (haml
          (:tr
           (:td (number->string i))
           (:td (call-with-output-string
                 (lambda (p)
                   (fprintf p "~.s" a))))))))))))

(define (display-raw-abstracts)
  (haml
   (:div
    (:h3 "Abstracts")
    (:ul
     ,@(for/list ([a (get/instance/abstracts* 'raw-abstracts)])
         (haml
          (:li
           (call-with-output-string
            (lambda (p)
              (fprintf p "~.s" (->jsexpr a)))))))))))

(define (store-abstracts as)
  ; Create three 'tables' (hashes):
  ; 1. One mapping an ID to the text of an abstract
  ; 2. One mapping each topic to the list of abstract IDs that match this topic
  ; 3. One mapping each topic to the list of abstract IDs that do NOT match this topic
  ; The third one is necessary, since the fact that we did not categorize an abstract as being about some topic, say "Sports", is not a guarantee that it is really not about "Sports", but simply that it is first and foremost about something else.
  ; To do this efficiently, use an exogenous list of topics, iterate over the abstracts and update the hashes for each abstract, creating the ID on the fly.
  (define-values (abstracts topics non-topics)
    (for/fold ([abstracts (hash)]
               [topics (hash)]
               [non-topics (hash)])
              ([a as]
               [i (in-naturals)])
      (match-define (abstract text categories non-categories) a)
      (define cs
        (map
         (compose1 string-downcase string-trim)
         (string-split categories ";")))
      (define non-cs
        (map
         (compose1 string-downcase string-trim)
         (string-split non-categories ";")))
      (values (hash-set abstracts i (string-trim text))
              (for/fold ([ts topics])
                        ([c cs])
                (hash-update ts c (lambda (v) (cons i v)) '()))
              (for/fold ([non-ts non-topics])
                        ([non-c non-cs])
                (hash-update non-ts non-c (lambda (v) (cons i v)) '())))))
  (put/instance/abstracts* 'abstracts abstracts)
  (put/instance/abstracts* 'topics topics)
  (put/instance/abstracts* 'non-topics non-topics))

(define (check-abstracts)
  (define abstracts (get/instance/abstracts* 'raw-abstracts))
  (page
   (haml
    (.container
     (:h1 "Check Abstracts")

     (:p "If you are fine with the abstracts, click 'Continue'. If you want to upload other abstracts, click 'Upload Abstracts' instead, which will overwrite the existing abstracts.")

     (button
      (lambda ()
        (store-abstracts abstracts))
      "Keep Abstracts")

     (button void "Upload other abstracts" #:to-step-id 'upload-abstracts)

     (display-raw-abstracts)))))

(define (show-abstracts)
  (page
   (haml
    (.container

     (button void "Finish Abstract Setup")

     (display-abstracts)))))

(define abstracts-admin
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

; FIXME: Relies on being exactly in this category
(define (abstract-task/page abs-task i total cat)
  (match-define (list _id a category) abs-task)
  (define (categorize in/out)
    (define correct-answer
      (if (string=? category cat)
          'in
          'out))
    (define correct-answer?
      (equal? in/out correct-answer))
    (if correct-answer?
        (put 'correct-answers (add1 (get 'correct-answers 0)))
        (put 'incorrect-answers (add1 (get 'incorrect-answers 0))))
    (put 'completed-answers
         (cons (list in/out correct-answer? abs-task)
               (get 'completed-answers '()))))

  (page
   (haml
    (.container
     (:h2 (format "Categorize Abstract ~a out of ~a" i total))
     (:p (format "Decide whether this abstract is about ~a or not." (string-titlecase cat)))

     ; FIXME: use cat an non-cat to check if the answer is right.
     (:h4 "The Abstract")
     (:p a)
     (button (lambda () (categorize 'in)) (string-titlecase cat))
     (button (lambda () (categorize 'out)) "Other")))))

(define (make-abstract-tasks)
  (define abstracts (get 'abstracts-to-do))
  (define total (length abstracts))
  (define category (get 'category))
  (for/study #:substudies ([(abs-task i) (in-indexed (in-list abstracts))])
    (put-current-round-name (format "abstract ~s" i))
    (abstract-task/page abs-task i total category)))

; NOTE: This assumes that all choices are of the form "Category" vs
; "Other" (i.e. "Not Category") Generalize if we ever need it.
(define (sample-work-abstracts cat n)
  (define cat-proportion 0.3) ; proportion of abstracts that should be in the category
  (define n-cat (exact-round (* n cat-proportion)))
  (define abstract-texts
    (get/instance/abstracts* 'abstracts))
  (define all-cat-ids
    (hash-ref (get/instance/abstracts* 'topics) cat))
  (define all-other-ids
    (hash-ref (get/instance/abstracts* 'non-topics) cat))
  (shuffle
   (append
    (map (lambda (id)
           (list id (hash-ref abstract-texts id) cat))
         (take all-cat-ids n-cat))
    (map (lambda (id)
           (list id (hash-ref abstract-texts id) "Other"))
         (take all-other-ids (- n n-cat))))))

(define (random-abstract-matching cat)
  (car (sample-work-abstracts cat 1)))

(define (abstract-tasks)
  (define (initialize)
    (define n (get 'n))
    (define category (get 'category))
    (put 'abstracts-to-do (sample-work-abstracts category n))
    (skip))

  (make-study
   "abstract-tasks"
   #:requires '(n category non-category)
   #:transitions
   (transition-graph
    [initialize --> tasks --> ,(lambda () done)])
   (list
    (make-step 'initialize initialize)
    (make-step/study 'tasks make-abstract-tasks))))
