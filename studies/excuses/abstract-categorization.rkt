#lang at-exp racket/base

(require csv-reading
         koyo/haml
         racket/contract
         racket/format
         racket/generic
         racket/list
         racket/match
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
 abstract-examples
 abstract-task/page
 get-reasons-stats
 get-topics-stats
 random-abstracts/topic
 random-abstracts/non-topic
 reasons-admin
 get/abstracts*
 end-timer start-timer)

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

(define (get-reasons-stats)
  (define reasons-for
    (get/instance* 'reasons-for))
  (define reasons-against
    (get/instance* 'reasons-against))
  (define all-topics
    (remove-duplicates
     (append (hash-keys reasons-for) (hash-keys reasons-against))))
  (for/list ([t all-topics])
    (list t
          (length (hash-ref reasons-for t null))
          (length (hash-ref reasons-against t null)))))

(serializable-struct abstract [id text categories non-categories]
  #:transparent
  #:methods gen:jsexprable
  [(define (->jsexpr s)
     (match-define (abstract id text categories non-categories) s)
     (hash 'id (if id (number->string id) "NA")
           'abstract text
           'categories categories
           'non-categories non-categories))])

(struct exn:fail:->abstract exn:fail ())

(define (exn:fail:csv-reader? e)
  (and (exn:fail? e)
       (regexp-match? #rx"%csv-reading" (exn-message e))))

(define (->abstract r)
  (if (not (= 3 (length r)))
      (raise (exn:fail:->abstract (format "row does not contain exactly three items: ~a" r) (current-continuation-marks)))
      (abstract #f (car r) (cadr r) (caddr r))))

(define (yn-radios label)
  (cast-result
   (radios label '(("yes" . "Yes")
                   ("no"  . "No")))
   (lambda (s) (string=? s "yes"))))

(define/contract (abstract-examples a/in a/out)
  (-> abstract? abstract? any/c)
  (define-values (text/in cat/in text/out cat/out)
    (values (abstract-text a/in) (abstract-categories a/in)
            (abstract-text a/out) (abstract-non-categories a/out)))
  (page
   (haml
    (.container
     @:h1{Task Description}

     @:p{As mentioned before, the task you have to do in this study consists of categorizing abstracts (short summaries of research papers) by topics.}

    @:h3{Example Abstract for '@string-titlecase[cat/in]'}

    @:p{The following is an abstract that does belong to the category '@string-titlecase[cat/in]':}

    @:p.abstract{"@text/in"}

    @:h3{Example Abstract not in '@string-titlecase[cat/out]'}

    @:p{The following is an abstract that does @:strong{not} belong in the category '@string-titlecase[cat/out]':}

    @:p.abstract{"@text/out"}

    @button[void]{Continue}))))

(define (binding:csv-file->list f)
  (cast-result*
   f
   (compose1 (lambda (f) (csv->list (make-csv-reader f '((separator-chars #\;))))) binding:file/port-in)
   #:exn-predicate exn:fail:csv-reader?
   #:exn-handler (λ (_e) "error reading csv: we expect comma (,) not semicolon (;) as separator, and all columns with text.~n You may also want to check for the BOM.)")))

; TODO: This call is ugly, change interface to string these together more conveniently. Pass in association list of lambdas and exceptions?
(define (input-abstracts label)
  (~> (input-file label)
      binding:csv-file->list
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
        (put/instance/abstracts* 'raw-abstracts (if header? (cdr abstracts) abstracts))))

     (button void "Cancel" #:to-step-id 'cancel)))))

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
      (match-define (abstract id text categories non-categories) a)
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
     ; NOTE: The cancel doesn't undo the previous step where we stored stuff on 'raw-abstracts. That's probably fine.
     (button void "Cancel" #:to-step-id 'cancel)

     (display-raw-abstracts)))))

(define (cancel)
  (skip))

(define abstracts-admin
  (make-study
   "configure abstracts"
   #:transitions
   (transition-graph
    [upload-abstracts --> check-abstracts
                      --> ,(lambda ()
                             (put 'cancel? #f)
                             done)]
    [cancel --> ,(lambda ()
                   (put 'cancel? #t)
                   done)])
   #:provides '(cancel?)

   (list
    (make-step 'upload-abstracts upload-abstracts/page)
    (make-step 'check-abstracts check-abstracts)
    (make-step 'cancel cancel))))


(define ((reasons-admin #:step-on-cancel [cancel 'cancel]))

  (define (input-reasons label)
    (~> (input-file label)
        binding:csv-file->list))

  (page
   (haml
    (.container
     (:h1 "Upload Reasons")

     (formular
      (haml
       (:div
        (:div (#:reasons (input-reasons "Upload a csv file with reasons")))
        submit-button))

      (lambda (#:reasons reasons)
        (put/instance* 'raw-reasons reasons)
        (define-values (reasons-for reasons-against)
          (for/fold ([reasons-for (hash)]
                     [reasons-against (hash)])
                    ; Assumes that the csv file has a header
                    ([r (cdr reasons)])
            (match-define
              (list _question topic _avg _ranking dir text)
              r)
            (if (string=? dir "for")
                (values (hash-update
                         reasons-for
                         (string->symbol (string-downcase topic))
                         (λ (v)
                           (cons text v))
                         null)
                        reasons-against)
                (values reasons-for
                        (hash-update reasons-against
                                     (string->symbol (string-downcase topic))
                                     (λ (v)
                                       (cons text v))
                                     null)))))
        (put/instance* 'reasons-for reasons-for)
        (put/instance* 'reasons-against reasons-against)))

     (button void "Cancel" #:to-step-id cancel)))))

(define (start-timer)
  (put #:root '*timer* 'start-time (current-seconds)))

(define (end-timer)
  (define t (current-seconds))
  (put #:root '*timer* 'end-time t)
  (- t (get #:root '*timer* 'start-time)))

(define (save-abstract-duration id cat correct? batch-name)
  (put* #:root '*timer*
        'abstracts-duration
        (cons (list id cat correct? batch-name (end-timer))
              (get* #:root '*timer* 'abstracts-duration null))))

; FIXME: Relies on being exactly in this category
(define (abstract-task/page abs-task i total cat prefix batch-name #:real-stakes? [real-stakes? #t])

  (match-define (abstract id text category non-category) abs-task)
  (define (categorize in/out)
    (define correct-answer
      (cond [(string=? category cat) 'in]
            [(string=? non-category cat) 'out]
            [else
             (error "abstract ~a should either be in or not be in the category ~a, but neither applies")]))
    (define correct-answer?
      (equal? in/out correct-answer))
    (save-abstract-duration id cat correct-answer? batch-name)
    (define correct-answers-key
      (string->symbol (format "~a-correct-answers" prefix)))
    (define incorrect-answers-key
      (string->symbol (format "~a-incorrect-answers" prefix)))
    (cond [correct-answer?
           (put/abstracts*
            correct-answers-key
            (add1 (get/abstracts* correct-answers-key 0)))
           (when real-stakes?
             (put/abstracts*
              'total-correct-answers
              (add1 (get/abstracts* 'total-correct-answers 0))))]
          [else
           (put/abstracts*
            incorrect-answers-key
            (add1 (get/abstracts* incorrect-answers-key  0)))
           (when real-stakes?
             (put/abstracts*
              'total-incorrect-answers
              (add1 (get/abstracts* 'total-incorrect-answers 0))))])
    (put 'completed-answers
         (cons (list in/out correct-answer? abs-task)
               (get 'completed-answers '()))))

  (define correct-n
    (get/abstracts* 'total-correct-answers 0))
  (define incorrect-n
    (get/abstracts* 'total-incorrect-answers 0))
  (define max-wrong
    (get* 'max-wrong-abstracts 0))
  (eprintf "max-wrong: ~a~n~n" max-wrong)
  (start-timer)
  ; FIXME: I also should show the fail page once they have too many incorrect answers.
  (cond [(and real-stakes? (>= incorrect-n max-wrong))
         (page
          (haml
           (.container
            (:h1 "You categorized too many abstracts wrongly")

            (:p "We are sorry, but you categorized more more than ~a abstracts wrongly, and thus cannot complete the study."))))]

        [else
         (page
          (haml
           (.container
            (:h2 (format "~a: Categorize Abstract ~a out of ~a" batch-name (add1 i) total))

            (cond [(and real-stakes? (>= max-wrong incorrect-n (- max-wrong 5)))
                   (haml
                    (:p.alert (:strong "Watch out!") (format "You are within 5 wrong attempts of failing out of the study: you got ~a abstracts wrong and you can get at most ~a wrong!" incorrect-n max-wrong)))]

                  [(and real-stakes? (zero? (modulo (+ correct-n incorrect-n) 8)))
                   (haml
                    (:p.info (format "You categorized ~a abstracts incorrectly so far. Remember that you can miscategorize at most ~a abstracts (66%), otherwise you cannot complete the study." incorrect-n max-wrong)))]

                  [else
                   (haml
                    (:p ""))])

            (:h4 "Decide whether this abstract is about " (haml (:strong (string-titlecase cat))) " or not.")

            ; FIXME: use cat an non-cat to check if the answer is right.
            (:p.abstract text)
            (button (lambda () (categorize 'in)) (string-titlecase cat))
            (button (lambda () (categorize 'out)) "Other"))))]))

(define (random-abstracts n category #:on-topic? [on-topic? #t])
  (define abstract-texts
    (get/instance/abstracts* 'abstracts))
  (define abs-ids
    (hash-ref
     (get/instance/abstracts* (if on-topic? 'topics 'non-topics))
     category))
  (map (lambda (id)
         (abstract
          id
          (hash-ref abstract-texts id)
          (if on-topic? category "")
          (if on-topic? "" category)))
       (random-sample abs-ids n #:replacement? #f)))

(define (random-abstracts/topic n category)
  (random-abstracts n category #:on-topic? #t))

(define (random-abstracts/non-topic n category)
  (random-abstracts n category #:on-topic? #f))

;;;;; Implement the task for categorizing into A, or B, or both, or neither
;;;;;
;;;;; This requires at first drawing n appropriate abstracts and knowing exactly whether they are in that category or not.
;;;;; Which I right now do not know, since I often don't know for sure that a given abstract is NOT in category B.
;;;;;
;;;;; Alternatively: give them a bunch of abstracts and let them categorize into 10 categories, i.e. choose ALL of the 2 categories that it fits into.
;;;;; OK, this is the same as with A or B or both, but now I acknowledge that we don't know all the categories, and they get a + for every category that they get right that we had, as well as less of a plus for stating "Other", when that is compatible.
;;;;; Forget for now, this will take some time to set up. Just do more tasks for now.
