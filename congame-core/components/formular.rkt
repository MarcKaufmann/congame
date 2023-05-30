#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         forms
         koyo/haml
         (prefix-in m: marionette)
         racket/match
         racket/port
         (prefix-in bot: (submod "bot.rkt" actions))
         (prefix-in study: "study.rkt"))

(provide
 ~error
 ~errors
 ~all-errors
 make-put-form/cons
 make-put-form/cons-hash
 make-put-form/cons/instance
 make-put-form/hash
 formular
 formular-autofill
 map-validator
 checkbox
 radios
 select
 input-date
 input-file
 input-number
 input-range
 input-text
 input-time
 textarea)

(define (kwd->symbol kwd)
  (string->symbol (keyword->string kwd)))

(define (symbol->kwd sym)
  (string->keyword (symbol->string sym)))

(define put-form
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kwd (in-list kws)]
           [arg (in-list kw-args)])
       (study:put (kwd->symbol kwd) arg)))))

(define (make-put-form/cons-hash key)
  (make-keyword-procedure
   (lambda (kws kw-args)
     (define ht
       (for/hasheq ([kwd (in-list kws)]
                    [arg (in-list kw-args)])
         (values (kwd->symbol kwd) arg)))
     (study:put key (cons ht (study:get key null))))))

(define (make-put-form/cons)
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kwd (in-list kws)]
           [arg (in-list kw-args)])
       (define key (kwd->symbol kwd))
       (study:put key (cons arg (study:get key null)))))))

(define (make-put-form/cons/instance)
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for([kwd (in-list kws)]
          [arg (in-list kw-args)])
       (define key (kwd->symbol kwd))
       (study:put/instance key (cons arg (study:get/instance key null)))))))

(define (make-put-form/hash key)
  (make-keyword-procedure
   (lambda (kws kw-args)
     (define ht
       (for/hasheq ([kwd (in-list kws)]
                    [arg (in-list kw-args)])
         (values (string->symbol (keyword->string kwd)) arg)))
     (study:put key ht))))

(define-syntax (~error stx)
  (raise-syntax-error stx "the ~error form may only be used inside (formular ...)"))

(define-syntax (~errors stx)
  (raise-syntax-error stx "the ~errors form may only be used inside (formlar ...)"))

(define-syntax (~all-errors stx)
  (raise-syntax-error stx "the ~all-errors form may only be used inside (formlar ...)"))

;; Building up an intermediate representation of formualrs would allow
;; us to compose smaller formulars into larger ones.  We may want to
;; do this eventually if reusability becomes a concern.
(define-syntax (formular stx)
  (syntax-parse stx
    #:literals (~error ~errors ~all-errors)
    [(_ {~alt
         {~optional
          {~seq #:bot ([bot-id:id (bot-fld:keyword bot-value:expr) ...] ...)}}
         {~optional
          {~seq #:fields ([dynamic-field-id:id dynamic-field:expr] ...)}
          #:defaults ([(dynamic-field-id 1) null]
                      [(dynamic-field 1) null])}}
        ...
        form
        {~optional action-e})
     #:with rw (format-id stx "rw")
     #:with tbl (format-id stx "tbl")
     #:with ((kwd fld def) ...)
     (let loop ([stx #'form]
                [pairs null])
       (syntax-parse stx
         [(kwd:keyword fld)
          (cons #'(kwd fld "") pairs)]

         [(kwd:keyword fld {#:default def:expr})
          (cons #'(kwd fld def) pairs)]

         [(e ...)
          (apply append (map (λ (stx) (loop stx null))
                             (syntax->list #'(e ...))))]

         [_ pairs]))
     #:with (field-id ...)
     (for/list ([idx (in-naturals 1)]
                [kwd (in-list (syntax-e #'(kwd ...)))])
       (format-id kwd "input_~a" idx))
     #:with patched-form
     (let loop ([stx #'form])
       (syntax-parse stx
         #:literals (~error ~errors ~all-errors)
         [(~error kwd:keyword)
          #'(let ([entry (hash-ref tbl 'kwd)])
              (rw (car entry) (widget-errors)))]

         [(~errors kwd:keyword ...+)
          #:with (entry-id ...) (generate-temporaries #'(kwd ...))
          #'(let ([entry-id (hash-ref tbl 'kwd)] ...)
              (:div (rw (car entry-id) (widget-errors)) ...))]

         [(~all-errors)
          #'(rw "input_1" (widget-all-errors))]

         [{~or (kwd:keyword _)
               (kwd:keyword _ _)}
          #'(let ([entry (hash-ref tbl 'kwd)])
              (rw (car entry) ((cdr entry) 'widget)))]

         [(e ...)
          #`(#,@(map loop (syntax-e #'(e ...))))]

         [e:id
          #:when (memq (syntax->datum #'e)
                       (syntax->datum #'(dynamic-field-id ...)))
          #'(let ([entry (hash-ref tbl 'e)])
              (rw (car entry) ((cdr entry) 'widget)))]

         [e #'e]))
     #:with defaults
     (with-syntax ([(fld&def ...)
                    (apply
                     append
                     (for/list ([fld (in-list (syntax-e #'(field-id ...)))]
                                [def (in-list (syntax-e #'(def ...)))]
                                #:unless (string=? (syntax-e def) ""))
                       (define fld-str
                         (datum->syntax fld (symbol->string (syntax-e fld))))
                       (list fld-str def)))])
       #'(hash fld&def ...))

     (define maybe-dupe-kwd
       (for/fold ([counts (hash)]
                  [dupe-stx #f]
                  #:result dupe-stx)
                 ([kwd-stx (in-list (syntax-e #'(kwd ...)))])
         #:break dupe-stx
         (define kwd (syntax-e kwd-stx))
         (define counts*
           (hash-update counts kwd add1 0))
         (values counts* (and (> (hash-ref counts* kwd) 1) kwd-stx))))
     (when maybe-dupe-kwd
       (raise-syntax-error 'formular "duplicate field" stx maybe-dupe-kwd))

     (when (attribute bot-id)
       (define fld-kwds (sort (map syntax->datum (syntax-e #'(kwd ...))) keyword<?))
       (for ([bot-id-stx (in-list (syntax-e #'(bot-id ...)))]
             [bot-kwd-stxs (in-list (syntax-e #'((bot-fld ...) ...)))])
         (define bot-id (syntax->datum bot-id-stx))
         (define bot-kwds (map syntax->datum (syntax-e bot-kwd-stxs)))
         (for ([bot-kwd-stx (in-list (syntax-e bot-kwd-stxs))]
               [bot-kwd (in-list bot-kwds)])
           (unless (memq bot-kwd fld-kwds)
             (raise-syntax-error 'formular (format "bot ~a declares field ~a but there's no matching form field" bot-id bot-kwd) bot-kwd-stx)))
         (for ([fld-kwd (in-list fld-kwds)])
           (unless (memq fld-kwd bot-kwds)
             (raise-syntax-error 'formular (format "bot ~a does not declare field ~a" bot-id fld-kwd) bot-id-stx)))))

     #'(let ([action-fn {~? action-e put-form}]
             [field-id fld] ...
             [dynamic-field-id dynamic-field] ...)
         (let ([tbl (make-hasheq
                     (list
                      (cons 'kwd (cons (symbol->string 'field-id) field-id)) ...
                      (cons 'dynamic-field-id (cons (symbol->string 'dynamic-field-id) dynamic-field-id)) ...))])
           (study:form
            #:defaults defaults
            (form* ([field-id (field-id 'validator)]
                    ...
                    [dynamic-field-id (dynamic-field-id 'validator)]
                    ...)
              (cons
               (list 'kwd ... (symbol->kwd 'dynamic-field-id) ...)
               (list field-id ... dynamic-field-id ...)))
            (lambda (res)
              (define vals-by-kwd
                (for/hasheq ([k (in-list (car res))]
                             [v (in-list (cdr res))])
                  (values k v)))
              (define sorted-kwds
                (sort (car res) keyword<?))
              (define sorted-vals
                (for/list ([k (in-list sorted-kwds)])
                  (hash-ref vals-by-kwd k)))
              (keyword-apply action-fn sorted-kwds sorted-vals null))
            (lambda (rw)
              (haml
               (:div
                (~? (haml
                     (:meta
                      ([:name "formular-autofill"]
                       [:content (study:when-bot
                                  (call-with-output-string
                                   (lambda (out)
                                     (define meta
                                       (make-hasheq
                                        (list (cons 'bot-id
                                                    (make-hasheq
                                                     (list (cons (car (hash-ref tbl 'bot-fld)) bot-value) ...))) ...)))
                                     (write meta out))))]))))
                patched-form))))))]))

(define (formular-autofill bot-id)
  (define meta-el (bot:find "meta[name=formular-autofill]"))
  (unless meta-el
    (error 'formular-autofill "could not find autofill metadata"))
  (define meta (call-with-input-string (m:element-attribute meta-el "content") read))
  ;; Collect elts-to-click and elts-to-type as an optimization since
  ;; interacting with them all at once is much faster than interacting
  ;; with each one individually and waiting on their animations.
  (define-values (elts-to-click elts-to-type)
    (for/fold ([elts-to-click null]
               [elts-to-type  (hash)]
               #:result (values (reverse elts-to-click) elts-to-type))
              ([(field-id value) (hash-ref meta bot-id)] #:when value)
      (define field-el (bot:find (format "[name=~a]" field-id)))
      (unless field-el
        (error 'formular-autofill (format "could not find field ~a" field-id)))
      (define field-type (m:element-attribute field-el "type"))
      (case field-type
        [("text")
         (values elts-to-click (hash-set elts-to-type field-el value))]

        [("number")
         (if (number? value)
             (values elts-to-click (hash-set elts-to-type field-el (number->string value)))
             (error "number field has to contain a number, but received ~a" value))]

        [("checkbox")
         (values (cons field-el elts-to-click) elts-to-type)]

        [("radio")
         (define the-radio (bot:find (format "[name=~a][value='~a']" field-id value)))
         (values (cons the-radio elts-to-click) elts-to-type)]

        ; FIXME: Marc thinks the bot:find won't work, since we need to get the option field with [value='value'], which is a child of select with name [name=field-id]. The space between them should select the child.
        [("select")
         (define the-select (bot:find (format "[name=~a] [value='~a']" field-id value)))
         (values (cons the-select elts-to-click) elts-to-type)]

        [else
         (error 'formular-autofill (format "unhandled field type ~a" field-type))])))
  (bot:click-all elts-to-click)
  (bot:type-all elts-to-type)
  (m:element-click! (bot:find "button[type=submit]")))

(define ((map-validator proc input) meth)
  (match meth
    ['validator
     (ensure
      (input 'validator)
      (lambda (v)
        `(ok . ,(proc v))))]
    [_ (input meth)]))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((widget-all-errors) _name _value errors)
  (list
   (haml
    (:ul.errors
     ,@(for/list ([pair (in-list errors)])
         (haml (:li (cdr pair))))))))

; FIXME: Move these to `forms` package eventually
(define (widget-date #:attributes [attributes null])
  (widget-input #:type "date"
                #:attributes attributes))

(define (widget-time #:attributes [attributes null])
  (widget-input #:type "time"
                #:attributes attributes))

(define (widget-range #:attributes [attributes null])
  (widget-input #:type "range"
                #:attributes attributes))


;; validators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((checkbox [label #f]
                   #:required? [required? #t]
                   #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/boolean (cons/required? required? null))]

    ['widget
     (lambda (name value errors)
       (define elt
         ((widget-checkbox #:attributes attributes) name value errors))
       (if label
           (haml
            (.group
             (:label elt label)
             ,@((widget-errors) name value errors)))
           elt))]))

(define ((radios label
                 options
                 #:required? [required? #t]
                 #:validators [validators null]
                 #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (cons/required? required? validators))]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label.radio-group
          label
          ((widget-radio-group options #:attributes attributes) name value errors))
         ,@((widget-errors) name value errors))))]))

(define ((select label
                 options
                 #:required? [required? #t]
                 #:validators [validators null]
                 #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (cons/required? required? validators))]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label
          ((widget-select options #:attributes attributes) name value errors) label)
         ,@((widget-errors) name value errors))))]))

(define ((input-file [label #f]
                     #:required? [required? #t]
                     #:validators [validators null]
                     #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/file (cons/required? required? validators))]

    ['widget
     (lambda (name value errors)
       (define elt
         ((widget-file #:attributes attributes) name value errors))
       (if label
           (haml
            (.group
             (:label elt label)
             ,@((widget-errors) name value errors)))
           elt))]))

(define (make-number-input widget)
  (define ((proc [label #f]
                 #:min [min -inf.0]
                 #:max [max +inf.0]
                 #:step [step 1]
                 #:required? [required? #t]
                 #:validators [validators null]
                 #:attributes [attributes null]) meth)
    (match meth
      ['validator
       (apply ensure binding/number (cons/required? required? (list* (to-real) (range/inclusive min max) validators)))]
      ['widget
       (lambda (name value errors)
         (define elt
           ((widget #:attributes (append
                                  `((min ,(if (= min -inf.0) "" (number->string min)))
                                    (max ,(if (= max +inf.0) "" (number->string max)))
                                    (step ,(number->string step)))
                                  attributes))
            name value errors))
         (if label
             (haml
              (.group
               (:label elt label)
               ,@((widget-errors) name value errors)))
             elt))]))
  proc)

(define input-number
  (make-number-input widget-number))

(define input-range
  (make-number-input widget-range))

(define ((input-text [label #f]
                     #:required? [required? #t]
                     #:validators [validators null]
                     #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (cons/required? required? validators))]
    ['widget
     (lambda (name value errors)
       (define elt
         ((widget-text #:attributes attributes) name value errors))
       (if label
           (haml
            (.group
             (:label elt label)
             ,@((widget-errors) name value errors)))
           elt))]))

(define ((textarea label
                   #:required? [required? #t]
                   #:validators [validators null]
                   #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (cons/required? required? validators))]
    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label label ((widget-textarea #:attributes attributes) name value errors))
         ,@((widget-errors) name value errors))))]))

(define ((input-time [label #f]
                     #:required? [required? #t]
                     #:validators [validators null]
                     #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (cons/required? required? validators))]
    ['widget
     (lambda (name value errors)
       (define elt
         ((widget-time #:attributes attributes) name value errors))
       (if label
           (haml
            (.group
             (:label label elt)
             ,@((widget-errors) name value errors)))
           elt))]))

(define ((input-date [label #f]
                     #:required? [required? #t]
                     #:validators [validators null]
                     #:attributes [attributes null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (cons/required? required? validators))]
    ['widget
     (lambda (name value errors)
       (define elt
         ((widget-date #:attributes attributes) name value errors))
       (if label
           (haml
            (.group
             (:label label elt)
             ,@((widget-errors) name value errors)))
           elt))]))

;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cons/required? required? l)
  (cond
    [(string? required?) (cons (required #:message required?) l)]
    [required? (cons (required) l)]
    [else l]))

(module+ test
  (require rackunit
           web-server/http)
  (check-equal?
   (((map-validator string->symbol (input-text "example")) 'validator)
    (binding:form #"input" #"hello"))
   '(ok . hello))
  (check-equal?
   (((map-validator (λ (n) (* n n)) (input-number "example")) 'validator)
    (binding:form #"input" #"42"))
   '(ok . 1764)))
