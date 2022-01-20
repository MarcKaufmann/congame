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
 formular
 formular-autofill
 checkbox
 radios
 input-file
 input-number
 input-text
 input-textarea)

;; Building up an intermediate representation of formualrs would allow
;; us to compose smaller formulars into larger ones.  We may want to
;; do this eventually if reusability becomes a concern.
(define-syntax (formular stx)
  (syntax-parse stx
    [(_ (~optional
         (~seq #:bot ([bot-id:id (bot-fld:keyword bot-value:expr) ...] ...)))
        form action-e)
     #:with rw (format-id stx "rw")
     #:with tbl (format-id stx "tbl")
     #:with ((kwd fld) ...)
     (let loop ([stx #'form]
                [pairs null])
       (syntax-parse stx
         [(kwd:keyword fld)
          (cons #'(kwd fld) pairs)]

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
         [(kwd:keyword _)
          #'(let ([entry (hash-ref tbl 'kwd)])
              (rw (car entry) ((cdr entry) 'widget)))]

         [(e ...)
          #`(#,@(map loop (syntax-e #'(e ...))))]

         [e #'e]))

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

     #'(let ([action-fn action-e]
             [field-id fld] ...)
         (let ([tbl (make-hasheq
                     (list (cons 'kwd (cons (symbol->string 'field-id) field-id)) ...))])
           (study:form
            (form* ([field-id (field-id 'validator)] ...)
              (cons
               (list 'kwd ...)
               (list field-id ...)))
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

        [else
         (error 'formular-autofill (format "unhandled field type ~a" field-type))])))
  (bot:click-all elts-to-click)
  (bot:type-all elts-to-type)
  (m:element-click! (bot:find "button[type=submit]")))

(define ((checkbox label) meth)
  (match meth
    ['validator
     (ensure binding/boolean (required))]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label ((widget-checkbox) name value errors) label)
         ,@((widget-errors) name value errors))))]))

(define ((radios label options #:validators [validators null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (required) validators)]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label.radio-group
          label
          ((widget-radio-group options) name value errors))
         ,@((widget-errors) name value errors))))]))

(define ((input-file label #:validators [validators null]) meth)
  (match meth
    ['validator
     (apply ensure binding/file (required) validators)]

    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label
          ((widget-file) name value errors) label)
         ,@((widget-errors) name value errors))))]))

(define ((input-number label #:min [min -inf.0] #:max [max +inf.0] #:validators [validators null]) meth)
  (match meth
    ['validator
     (apply ensure binding/number (required) (to-real) (cons (range/inclusive min max) validators))]
    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label
          ((widget-number #:attributes `((min ,(if (= min -inf.0) "" (number->string min)))
                                         (max ,(if (= max +inf.0) "" (number->string max)))))
           name value errors)
          label)
         ,@((widget-errors) name value errors))))]))

(define ((input-text label #:validators [validators null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (required) validators)]
    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label
          ((widget-text) name value errors) label)
         ,@((widget-errors) name value errors))))]))

(define ((input-textarea label #:validators [validators null]) meth)
  (match meth
    ['validator
     (apply ensure binding/text (required) validators)]
    ['widget
     (lambda (name value errors)
       (haml
        (.group
         (:label
          ((widget-textarea) name value errors) label)
         ,@((widget-errors) name value errors))))]))
