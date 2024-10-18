#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre)
         forms
         json
         koyo/haml
         (prefix-in m: marionette)
         racket/list
         racket/match
         racket/port
         racket/string
         (prefix-in bot: (submod "bot.rkt" actions))
         (prefix-in study: "study.rkt")
         web-server/http)

(provide
 ~error
 ~errors
 ~all-errors
 make-put-form/cons
 make-put-form/cons-hash
 make-put-form/cons/instance
 make-put-form/hash
 put-form/with
 formular
 formular-autofill
 map-validator
 map-result
 map-result*
 checkbox
 radios
 select
 input-date
 input-file
 input-list
 input-number
 input-range
 input-text
 input-time
 textarea
 make-checkboxes
 make-sliders
 make-radios
 make-radios-with-other)

(define (kwd->symbol kwd)
  (string->symbol (keyword->string kwd)))

(define put-form
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kwd (in-list kws)]
           [arg (in-list kw-args)])
       (study:put (kwd->symbol kwd) arg)))))

(define (put-form/with p)
  (make-keyword-procedure
   (lambda (kws kw-args)
     (for ([kwd (in-list kws)]
           [arg (in-list kw-args)])
       (p (kwd->symbol kwd) arg)))))

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

(define (make-put-form/hash key #:putter [putter study:put])
  (make-keyword-procedure
   (lambda (kws kw-args)
     (define ht
       (for/hasheq ([kwd (in-list kws)]
                    [arg (in-list kw-args)])
         (values (string->symbol (keyword->string kwd)) arg)))
     (putter key ht))))

(define-syntax (~error stx)
  (raise-syntax-error stx "the ~error form may only be used inside (formular ...)"))

(define-syntax (~errors stx)
  (raise-syntax-error stx "the ~errors form may only be used inside (formular ...)"))

(define-syntax (~all-errors stx)
  (raise-syntax-error stx "the ~all-errors form may only be used inside (formular ...)"))

;; Building up an intermediate representation of formualrs would allow
;; us to compose smaller formulars into larger ones.  We may want to
;; do this eventually if reusability becomes a concern.
(define-syntax (formular stx)
  (define (id->keyword stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

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
     #:with ((var-id kwd fld def) ...)
     (let loop ([stx #'form]
                [pairs null])
       (syntax-parse stx
         #:literals (formular set!)
         [(formular . _)
          (raise-syntax-error 'formular "cannot nest formular forms" stx)]

         [(set! id:id fld)
          #:with kwd (id->keyword #'id)
          (cons #'(id kwd fld "") pairs)]

         [(kwd:keyword fld)
          (cons #'(#f kwd fld "") pairs)]

         [(kwd:keyword fld {#:default def:expr})
          (cons #'(#f kwd fld def) pairs)]

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
       ;; FIXME: Detect uses of (~errors foo) where foo is not actually known.
       (syntax-parse stx
         #:literals (~error ~errors ~all-errors set!)
         [(~error id:id)
          #:with kwd (id->keyword #'id)
          (loop #'(~error kwd))]

         [(~error kwd:keyword)
          #'(let ([entry (hash-ref tbl 'kwd)])
              (rw (car entry) (widget-errors)))]

         [(~errors id:id ...+)
          #:with (kwd ...) (map id->keyword (syntax-e #(id ...)))
          (loop #'(~errors kwd ...))]

         [(~errors kwd:keyword ...+)
          #:with (entry-id ...) (generate-temporaries #'(kwd ...))
          #'(let ([entry-id (hash-ref tbl 'kwd)] ...)
              (:div (rw (car entry-id) (widget-errors)) ...))]

         [(~all-errors)
          #'(rw "input_1" (widget-all-errors))]

         [(set! id:id _)
          #:with kwd (id->keyword #'id)
          #'(let ([entry (hash-ref tbl 'kwd)])
              (let ([widget (formular-field-widget (cdr entry))])
                (rw (car entry) widget)))]

         [{~or (kwd:keyword _)
               (kwd:keyword _ _)}
          #'(let ([entry (hash-ref tbl 'kwd)])
              (let ([widget (formular-field-widget (cdr entry))])
                (rw (car entry) widget)))]

         [(e ...)
          #`(#,@(map loop (syntax-e #'(e ...))))]

         [e:id
          #:when (memq (syntax->datum #'e)
                       (syntax->datum #'(dynamic-field-id ...)))
          #:with kwd (datum->syntax #'e (string->keyword (symbol->string (syntax-e #'e))))
          #'(let ([entry (hash-ref tbl 'kwd)])
              (let ([widget (formular-field-widget (cdr entry))])
                (rw (car entry) widget)))]

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
     #:with (dynamic-field-kwd ...)
     (for/list ([stx (in-list (syntax-e #'(dynamic-field-id ...)))])
       (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

     #:with default-action
     (if (ormap values (syntax->datum #'(var-id ...)))
         (with-syntax ([((kwd tmp) ...)
                        (for/list ([kwd-stx (in-list (syntax-e #'(kwd ...)))]
                                   [tmp-stx (in-list (generate-temporaries #'(var-id ...)))])
                          (list kwd-stx tmp-stx))])
           #'(lambda ({~@ kwd tmp} ...)
               (set! var-id tmp) ...))
         #'put-form)

     (when (and (ormap values (syntax->datum #'(var-id ...)))
                (syntax-e #'{~? action-e #f}))
       (raise-syntax-error 'formular "forms with set!-based fields cannot have custom actions" stx #'action-e))

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
       (define fld-kwds
         (sort
          (append
           (map syntax->datum (syntax-e #'(kwd ...)))
           (map syntax->datum (syntax-e #'(dynamic-field-kwd ...))))
          keyword<?))
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

     #'(let ([action-fn {~? action-e default-action}]
             [field-id fld] ...
             [dynamic-field-id dynamic-field] ...)
         (let ([tbl (make-hasheq
                     (list
                      (cons 'kwd (cons (symbol->string 'field-id) field-id)) ...
                      (cons 'dynamic-field-kwd (cons (symbol->string 'dynamic-field-id) dynamic-field-id)) ...))])
           (study:form
            #:defaults defaults
            #:combine (λ (_k v1 v2)
                        (if (pair? v1)
                            (cons v2 v1)
                            (list v2 v1)))
            (form* ([field-id (formular-field-validator field-id)]
                    ...
                    [dynamic-field-id (formular-field-validator dynamic-field-id)]
                    ...)
              (cons
               (list 'kwd ... 'dynamic-field-kwd ...)
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
               [elts-to-type (hash)]
               #:result (values (reverse elts-to-click) elts-to-type))
              ([(field-id value) (hash-ref meta bot-id)] #:when value)
      (define field-el (bot:find (format "[name=~a]" field-id)))
      (unless field-el
        (error 'formular-autofill (format "could not find field ~a" field-id)))
      (define field-type
        (or
         (m:element-attribute field-el "type")
         (m:element-tag field-el)))
      (case field-type
        [("text" "textarea")
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

        [("select")
         (define the-select (bot:find (format "[name=~a]" field-id)))
         (define the-option (bot:element-find the-select (format "[value='~a']" value)))
         (values (cons the-option elts-to-click) elts-to-type)]

        [else
         (error 'formular-autofill (format "unhandled field type ~a" field-type))])))
  (bot:click-all elts-to-click)
  (bot:type-all elts-to-type)
  (m:element-click! (bot:find "button[type=submit]")))

(define (map-validator f proc)
  (struct-copy formular-field f [validator (ensure (formular-field-validator f) proc)]))

(define (map-result input proc)
  (map-validator input (λ (v) `(ok . ,(proc v)))))

(define (map-result* input proc
                     #:exn-predicate [exn-predicate exn:fail?]
                     #:exn-handler [exn-handler (λ (e) `(err . ,(exn-message e)))])
  (map-validator
   input
   (lambda (v)
     (with-handlers ([exn-predicate exn-handler])
       `(ok . ,(proc v))))))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((widget-all-errors) _name _value errors) ;; noqa
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

(struct formular-field
  (validator widget expected-values))

(define (make-formular-field
         #:validator validator
         #:widget widget
         #:expected-values [expected-values 1])
  (formular-field
   #;validator validator
   #;widget widget
   #;expected-values expected-values))

(define (checkbox [label #f]
                  #:required? [required? #t]
                  #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/boolean (cons/required? required? null))
   #:widget
   (lambda (name value errors)
     (define elt
       ((widget-checkbox #:attributes attributes) name value errors))
     (if label
         (haml
          (.group
           (:label elt label)
           ,@((widget-errors) name value errors)))
         elt))))

(define (radios label
                options
                #:required? [required? #t]
                #:validators [validators null]
                #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (haml
      (.group
       (:label.radio-group
        label
        ((widget-radio-group options #:attributes attributes) name value errors))
       ,@((widget-errors) name value errors))))))

(define (select label
                options
                #:required? [required? #t]
                #:validators [validators null]
                #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (haml
      (.group
       (:label
        ((widget-select options #:attributes attributes) name value errors) label)
       ,@((widget-errors) name value errors))))))

(define (input-file [label #f]
                    #:required? [required? #t]
                    #:validators [validators null]
                    #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/file (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (define elt
       ((widget-file #:attributes attributes) name value errors))
     (if label
         (haml
          (.group
           (:label elt label)
           ,@((widget-errors) name value errors)))
         elt))))

(define ((make-number-input widget)
         [label #f]
         #:min [min -inf.0]
         #:max [max +inf.0]
         #:step [step 1]
         #:required? [required? #t]
         #:validators [validators null]
         #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/number (cons/required? required? (list* (to-real) (range/inclusive min max) validators)))
   #:widget
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
         elt))))

(define input-number
  (make-number-input widget-number))

(define input-range
  (make-number-input widget-range))

(define (input-text [label #f]
                    #:required? [required? #t]
                    #:validators [validators null]
                    #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (define elt
       ((widget-text #:attributes attributes) name value errors))
     (if label
         (haml
          (.group
           (:label elt label)
           ,@((widget-errors) name value errors)))
         elt))))

(define (textarea label
                  #:required? [required? #t]
                  #:validators [validators null]
                  #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (haml
      (.group
       (:label label ((widget-textarea #:attributes attributes) name value errors))
       ,@((widget-errors) name value errors))))))

(define (input-time [label #f]
                    #:required? [required? #t]
                    #:validators [validators null]
                    #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (define elt
       ((widget-time #:attributes attributes) name value errors))
     (if label
         (haml
          (.group
           (:label label elt)
           ,@((widget-errors) name value errors)))
         elt))))

(define (input-date [label #f]
                    #:required? [required? #t]
                    #:validators [validators null]
                    #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (define elt
       ((widget-date #:attributes attributes) name value errors))
     (if label
         (haml
          (.group
           (:label label elt)
           ,@((widget-errors) name value errors)))
         elt))))

(define (input-list fields)
  (define n
    (for/sum ([f (in-list fields)])
      (formular-field-expected-values f)))
  (make-formular-field
   #:expected-values n
   #:validator
   (ensure
    binding/list
    (list-longer-than n)
    (λ (vals) (ok (reverse vals)))
    (lambda (vals)
      (for/fold ([ress null]
                 [errs null]
                 #:result
                 (if (ormap non-empty-string? errs)
                     (err (reverse errs))
                     (ok (reverse ress))))
                ([f (in-list fields)]
                 [g (in-list (get-val-groups fields vals))])
        (define bindings
          (if (pair? g)
              (for/list ([val (in-list g)])
                (binding:form #"" (string->bytes/utf-8 val)))
              (binding:form #"" (string->bytes/utf-8 g))))
        (define f-res
          ((formular-field-validator f) bindings))
        (if (ok? f-res)
            (values (cons (cdr f-res) ress) (cons "" errs))
            (values (cons "" ress) (cons (cdr f-res) errs))))))
   #:widget
   (lambda (name maybe-vals errors)
     (define sym (string->symbol name))
     (define val-groups
       (let ([vals (or maybe-vals (make-list n #f))])
         (get-val-groups fields (reverse vals))))
     (define these-errors
       (assq sym errors))
     (define err-groups
       (if these-errors
           (cdr these-errors)
           (make-list n #f)))
     (let ([errors (remq these-errors errors)])
       (haml
        (.input-list-group
         ,@(for/list ([f (in-list fields)]
                      [v (in-list val-groups)]
                      [e (in-list err-groups)])
             (let ([errors (if (and e (not (equal? e "")))
                               (cons (cons sym e) errors)
                               errors)])
               ((formular-field-widget f) name v errors)))))))))

(define (get-val-groups fields vals)
  (for/fold ([vals vals] [groups null] #:result (reverse groups))
            ([f (in-list fields)])
    (define n (formular-field-expected-values f)) ;; noqa
    (define g
      (if (= n 1)
          (car vals)
          (take vals n)))
    (values
     (drop vals n)
     (cons g groups))))

(define ((list-longer-than [n 0] [message (format "You must select ~a or more items." n)]) xs)
  (let ([xs (or xs null)])
    (if (>= (length xs) n)
        (ok xs)
        (err message))))

(define (make-checkboxes options
                         render-proc
                         #:n [n 0]
                         #:message [message #f]
                         #:validators [validators null]
                         #:attributes [attributes null])
  (make-formular-field
   #:expected-values n
   #:validator
   (apply ensure
          binding/list
          (if message
              (cons (list-longer-than n message) validators)
              (cons (list-longer-than n) validators)))
   #:widget
   (lambda (name value errors)
     (define (make-checkbox option [label ""])
       (define the-values
         (and value
              (for/list ([bind (in-list (if (pair? value) value (list value)))])
                (string->symbol (bytes->string/utf-8 (binding:form-value bind))))))
       (define attributes*
         (if (and the-values (memq option the-values))
             (cons '(checked "") attributes)
             attributes))
       `(label
         (input
          ([name ,name]
           [type "checkbox"]
           [value ,(symbol->string option)]
           ,@attributes*))
         ,label))
     (haml
      (.group
       (render-proc options make-checkbox)
       ,@((widget-errors) name value errors))))))

(define (make-sliders n render-proc
                      #:message [message #f]
                      #:validators [validators null])
  (make-formular-field
   #:expected-values n
   #:validator
   (apply ensure
          binding/list
          (λ (vs) (ok (reverse vs)))
          (if message
              (cons (list-longer-than n message) validators)
              (cons (list-longer-than n) validators)))
   #:widget
   (lambda (name value errors)
     (define the-values
       (make-vector n #f))
     (when value
       (for ([(bind idx) (in-indexed (in-list (if (pair? value) value (list value))))])
         (define this-value (string->number (bytes->string/utf-8 (binding:form-value bind))))
         (vector-set! the-values idx this-value)))
     (haml
      (.group
       ,@(for/list ([(this-value idx) (in-indexed (in-vector the-values))])
           (render-proc idx name this-value))
       ,@((widget-errors) name value errors))))))

(define (make-radios options
                     render-proc
                     #:required? [required? #t]
                     #:validators [validators null]
                     #:attributes [attributes null])
  (make-formular-field
   #:validator
   (apply ensure binding/text (cons/required? required? validators))
   #:widget
   (lambda (name value errors)
     (define (make-radio option [label ""])
       (define the-value (and value (string->symbol (bytes->string/utf-8 (binding:form-value value)))))
       (define attributes*
         (if (eq? the-value option)
             (cons '(checked "") attributes)
             attributes))
       `(label
         (input
          ([name ,name]
           [type "radio"]
           [value ,(symbol->string option)]
           ,@attributes*))
         ,label))
     (haml
      (.group
       (render-proc options make-radio)
       ,@((widget-errors) name value errors))))))

(define (make-radios-with-other options
                                #:required? [required? #t]
                                #:validators [validators null])
  (make-formular-field
   #:validator
   (apply ensure
          binding/text
          (cons/required?
           required?
           (cons
            (lambda (json-data)
              (if (not json-data)
                  (ok #f)
                  (with-handlers ([exn:fail? (λ (_) (err "failed to parse radio JSON"))])
                    (define ht (string->jsexpr json-data))
                    (define radio-value (json-null->string (hash-ref ht 'radioValue (json-null))))
                    (define other-value (json-null->string (hash-ref ht 'otherValue (json-null))))
                    (cond
                      [(and (equal? radio-value "")
                            (equal? other-value ""))
                       (err "You must pick a value or write something in the 'Other' field.")]
                      [(equal? other-value "")
                       (ok radio-value)]
                      [else
                       (ok other-value)]))))
            validators)))
   #:widget
   (lambda (name value errors)
     (haml
      (.group
       (:cg-radios-with-other
        ([:name name]
         [:options (jsexpr->string
                    (for/list ([opt (in-list options)])
                      (match-define (cons value label) opt)
                      (hasheq 'value (symbol->string value) 'label label)))]
         [:value "{\"radioValue\": null, \"otherValue\": null}"]))
       ,@((widget-errors) name value errors))))))

(define (json-null->string v)
  (if (eq? v (json-null)) "" v))

; FIXME: Marc added - but not sure if/where it is used

(provide
 is-equal)

(define ((is-equal a #:message [message #f]) v)
  (if (equal? v a)
      (ok v)
      (err (or message (format "Should be equal to ~a" a)))))

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
   ((formular-field-validator (map-result (input-text "example") string->symbol))
    (binding:form #"input" #"hello"))
   '(ok . hello))
  (check-equal?
   ((formular-field-validator (map-result (input-number "example") (λ (n) (* n n))))
    (binding:form #"input" #"42"))
   '(ok . 1764)))

;; common tools, helpers, utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ tools
  (provide
   submit-button
   submit-button/label
   input-likert
   input-patience
   input-risk
   select-scale)

  (define submit-button
    (haml
     (:button.button.next-button ([:type "submit"]) "Submit")))

  (define (submit-button/label label)
    (haml
     (:button.button.next-button ([:type "submit"]) label)))

  (define (select-scale label from to)
    (select label
            (cons (cons "" "--Please choose an option--")
                  (for/list ([n (in-list (range from (add1 to)))])
                    (define n-string (number->string n))
                    (cons n-string (format " ~a " n-string))))))

  ;; The following are from Falk, Becker, Dohmen, Huffman, Sunde 2022, MS
  (define (input-patience)
    (select-scale "In comparison with others, are you a person who is generally willing to give up something today in order to benefit from that in the future? (0 means very unwilling, 10 means very willing.)" 0 10))

  (define (input-risk)
    (select-scale "Are you a person who is generally willing to take risks, or do you try to avoid taking risks? (0 means you avoid risk as much as possible, 10 means you are very willing to take risks.)" 0 10))

  (define (input-likert label)
    (select label
            `((""  . "--Please choose an option--")
              ("1" . " 1 ")
              ("2" . " 2 ")
              ("3" . " 3 ")
              ("4" . " 4 ")
              ("5" . " 5 ")
              ("6" . " 6 ")
              ("7" . " 7 ")))))
