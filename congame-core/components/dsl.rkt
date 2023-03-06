#lang racket/base

(require (for-syntax racket/base)
         koyo/haml
         racket/list
         racket/string
         racket/syntax
         (prefix-in s: scribble/reader)
         syntax/parse

         "dsl/runtime.rkt"

         "formular.rkt"
         "registry.rkt"
         "study.rkt"
         "transition-graph.rkt")

(provide
 dsl-require
 read-syntax+compile)

(define attached-mods
  '(congame/components/dsl/runtime
    congame/components/formular
    congame/components/registry
    congame/components/study
    congame/components/transition-graph
    koyo/haml
    racket/format
    racket/string))

(define (dsl-require s id)
  (define in (open-input-string s))
  (port-count-lines! in)
  (define ns (current-namespace))
  (define mods-to-attach
    (append attached-mods (map module-path-index-resolve (registered-study-mod-path-indexes))))
  (define mods-to-require
    (append attached-mods (registered-study-mod-paths)))
  (parameterize ([current-namespace (make-base-namespace)])
    (for ([mod (in-list mods-to-attach)])
      (namespace-attach-module ns mod))
    (for ([mod (in-list mods-to-require)])
      (eval `(require ,mod)))
    (eval `(define *env* (make-initial-environment)))
    (eval `(define current-study-scripts (make-parameter null)))
    (eval `(define current-study-styles (make-parameter null)))
    (for-each eval (syntax->datum (read-syntax+compile 'dsl in)))
    (namespace-variable-value id)))

(define (read-syntax what in)
  (s:read-syntax-inside what in))

(define (read-syntax+compile what in)
  (compile-module (read-syntax what in)))

;; compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Env (actions imports templates)
  #:transparent)

(define current-env
  (make-parameter #f))

(define current-in-template?
  (make-parameter #f))

(define (build-env stxs)
  (define-values (_names actions imports templates)
    (for/fold ([names null]
               [actions null]
               [imports null]
               [templates (hash)])
              ([stx (in-list stxs)])
      (syntax-parse stx
        #:datum-literals (action import step study template template-ungrouped)
        [(action id:id _ ...)
         (define name
           (syntax->datum #'id))
         (when (memq name names)
           (raise-syntax-error 'dsl "identifier already defined" stx #'id))
         (values
          (cons name names)
          (cons name actions)
          imports
          templates)]
        [(import _ id:id ...+)
         (define name-stxes
           (syntax-e #'(id ...)))
         (define import-names
           (map syntax->datum name-stxes))
         (define defined-name
           (ormap (λ (n) (and (memq n names) n)) import-names))
         (when defined-name
           (raise-syntax-error 'dsl "indentifier already defined" stx (list-ref name-stxes (index-of import-names defined-name))))
         (values
          (append import-names names)
          actions
          (append import-names imports)
          templates)]
        [({~or step study} id:id _ ...)
         (define name
           (syntax->datum #'id))
         (when (memq name names)
           (raise-syntax-error 'dsl "identifier already defined" stx #'id))
         (values
          (cons name names)
          actions
          imports
          templates)]
        [(template id:id _ ...)
         (define name
           (syntax->datum #'id))
         (when (memq name names)
           (raise-syntax-error 'dsl "identifier already defined" stx #'id))
         (values
          (cons name names)
          actions
          imports
          (hash-set templates (syntax->datum #'id) 'grouped))]
        [(template-ungrouped id:id _ ...)
         (define name
           (syntax->datum #'id))
         (when (memq name names)
           (raise-syntax-error 'dsl "identifier already defined" stx #'id))
         (values
          (cons name names)
          actions
          imports
          (hash-set templates (syntax->datum #'id) 'ungrouped))]
        [_
         (values names actions imports templates)])))
  (Env actions imports templates))

(define (compile-module stx)
  (syntax-parse stx
    [(statement ...)
     (parameterize ([current-env (build-env (syntax-e #'(statement ...)))])
       (with-syntax ([(compiled-stmt ...) (filter-map compile-stmt (syntax-e #'(statement ...)))])
         #'(compiled-stmt ...)))]))

(define (compile-stmt stx)
  (define-syntax-class next-step-expr
    #:datum-literals (end)
    (pattern end
             #:with target #'done)
    (pattern next-step-id:id
             #:with target #'(goto next-step-id)))

  (define-syntax-class transition-entry
    #:datum-literals (--> end ev)
    (pattern -->
             #:attr step-name #f
             #:attr step-id #f
             #:with compiled this-syntax)

    (pattern id:id
             #:with step-name #'id
             #:with step-id #'id
             #:with compiled this-syntax)

    (pattern (ev e)
             #:attr step-name #f
             #:attr step-id #f
             #:with compiled-expr (compile-expr this-syntax)
             #:with compiled #'(unquote compiled-expr))

    (pattern [step-name:id step-id:id]
             #:with compiled #'step-name))

  (syntax-parse stx
    #:datum-literals (action define import script step study style template template-ungrouped)
    [s:string
     #:when (regexp-match? #px"^\\s+$" (syntax->datum #'s))
     #f]

    [(action name:id content ...+)
     #:with (compiled-content ...) (filter-map compile-action-expr (syntax-e #'(content ...)))
     #'(define (name)
         (let ([*env* (make-environment *env*)])
           compiled-content ...))]

    [(define name:id e:expr)
     #'(interpret '(define name e) *env*)]

    [(import mod-path:id id:id)
     #'(environment-set! *env* 'id (study-mod-require 'mod-path 'id))]

    [(import mod-path:id id:id ...+)
     #:with (compiled-import ...) (map compile-stmt (syntax-e #'((import mod-path id) ...)))
     #'(begin compiled-import ...)]

    [(step name:id {~optional {~seq #:pre pre-action-id:id}} content ...+)
     #:fail-when (eq? (syntax-e #'name) 'end) "'end' is not a valid step id"
     #:with (compiled-content ...) (map compile-markup (syntax-e (group-by-paragraph #'(content ...))))
     #'(define (name)
         (let ([*env* (make-environment *env*)])
           {~? (pre-action-id)}
           (page
            (haml
             (.container
              ,@(->styles (reverse (current-study-styles)))
              compiled-content ...
              ,@(->scripts (reverse (current-study-scripts))))))))]

    [(script content:string ...+)
     #'(current-study-scripts
        (let ([old (current-study-scripts)])
          (cons (string-append content ...) old)))]

    [(study study-id:id #:transitions [transition-entry:transition-entry ...] ...+)
     #:fail-when (eq? (syntax-e #'study-id) 'end) "'end' is not a valid study id"
     #:with study-id-str (datum->syntax #'study-id (symbol->string (syntax->datum #'study-id)))
     #:with ((step-name step-id) ...)
     (for/fold ([stxes null]
                [seen-names (hasheq)]
                #:result (reverse stxes))
               ([name-stx (in-list (syntax-e #'({~? transition-entry.step-name} ... ...)))]
                [id-stx (in-list (syntax-e #'({~? transition-entry.step-id} ... ...)))])
       (define name
         (and name-stx (syntax->datum name-stx)))
       (cond
         [(hash-has-key? seen-names name)
          (values stxes seen-names)]
         [else
          (values (cons (list (or name-stx id-stx) id-stx) stxes)
                  (hash-set seen-names name #t))]))
     #'(define study-id
         (make-study
          study-id-str
          #:transitions (transition-graph [transition-entry.compiled ...] ...)
          (list (->step 'step-name step-id) ...)))]

    [(study study-id:id #:dynamic e:expr)
     #:fail-when (eq? (syntax-e #'study-id) 'end) "'end' is not a valid study id"
     #'(define study-id (interpret 'e *env*))]

    [(style content:string ...+)
     #'(current-study-styles
        (let ([old (current-study-styles)])
          (cons (string-append content ...) old)))]

    [({~and {~or template template-ungrouped} form-id} template-id:id content ...+)
     #:with (compiled-content ...) (parameterize ([current-in-template? #t]
                                                  [current-paragraph-tags
                                                   (if (eq? (syntax-e #'form-id) 'template-ungrouped)
                                                       (cons 'yield (current-paragraph-tags))
                                                       (current-paragraph-tags))])
                                     (map compile-markup (syntax-e (group-by-paragraph #'(content ...)))))
     #'(define (template-id content-proc)
         (let ([*env* (make-environment *env*)])
           (haml (:div compiled-content ...))))]))

;; @style{  }

(define (compile-markup stx)
  (define-syntax-class body
    (pattern body #:with compiled (compile-markup #'body)))

  (define-syntax-class list-item
    #:datum-literals (li)
    #:attributes ([xexpr 1])
    (pattern "\n"
             #:with (xexpr ...) null)
    (pattern (li body:body ...+)
             #:with (xexpr ...) #'((:li body.compiled ...)))
    (pattern e:expr
             #:with (xexpr ...) #`(#,(compile-expr #'e))))

  (define-splicing-syntax-class attrs
    (pattern {~seq {~alt
                    {~optional {~seq #:id id-e:string}}
                    {~optional {~seq #:class class-e:string}}
                    {~optional {~seq #:style style-e:string}}} ...}
             #:with (e ...) #'({~? {~@ [:id id-e]}}
                               {~? {~@ [:class class-e]}}
                               {~? {~@ [:style style-e]}})
             #:with (group ...) (if (null? (syntax-e #'(e ...)))
                                    #'()
                                    #'(({~@ e ...})))))

  ;; NOTE: For block-style tags, it's the tag's responsibility to call
  ;; group-by-paragraph on its exprs.
  (syntax-parse stx
    #:datum-literals (a br button div em form h1 h2 h3 hl img ol p quote refresh-every span strong
                        table tbody td th thead tr
                        template u1 ul yield
                        script style)
    [num:number #'num]
    [str:string #'str]
    [kwd:keyword #'kwd]
    [(quote e) #''e]

    [({~and {~or hl u1} id} . _rest)
     #:fail-when #t (apply format
                           "did you mean ~a (~a)?"
                           (hash-ref
                            (hasheq
                             'hl '(h1 "the character '1' (one) instead of 'l' (lowercase L)")
                             'u1 '(ul "the character 'l' (lowercase L) instead of '1' (one)"))
                            (syntax->datum #'id)))
     #'(void)]

    [(a ~! attrs:attrs url:string body:body ...+)
     #'(:a ([:href url] {~@ attrs.e ...}) body.compiled ...)]

    [(br ~!)
     #'(:br)]

    [(button ~!
             {~alt
              {~optional {~seq #:action action:id}}
              {~optional {~seq #:to-step step-id:id}}} ...
             text0:string text:string ...)
     #:with joined-text (datum->syntax #'text0 (string-join (syntax->datum #'(text0 text ...)) ""))
     (check-action-id 'button #'{~? action #f})
     #'(button
        {~? {~@ #:to-step-id 'step-id}}
        {~? action void}
        joined-text)]

    [(div ~! attrs:attrs body ...+)
     #:with (compiled-body ...) (map compile-markup (syntax-e (group-by-paragraph #'(body ...))))
     #'(:div attrs.group ... compiled-body ...)]

    [(em ~! attrs:attrs body:body ...+)
     #'(:em attrs.group ... body.compiled ...)]

    [(form ~! {~optional {~seq #:action action:id}} body ...+)
     #:with ((compiled-body ...) ...) (map compile-form-expr (syntax-e (group-by-paragraph #'(body ...))))
     (check-action-id 'form #'{~? action #f})
     #'(formular
        (haml
         (:div
          compiled-body ... ...))
        (make-put-all-keywords {~? action void}))]

    [({~and {~or h1 h2 h3} tag} ~! attrs:attrs body:body ...+)
     #:with tag-id (format-id #'tag ":~a" #'tag)
     #'(tag-id attrs.group ... body.compiled ...)]

    [({~and {~or table tbody thead td th tr} tag} ~! attrs:attrs body:body ...)
     #:with tag-id (format-id #'tag ":~a" #'tag)
     #'(tag-id attrs.group ... body.compiled ...)]

    [(img ~! attrs:attrs url:string)
     #'(:img ([:src url] {~@ attrs.e ...}))]

    [(p ~! attrs:attrs body:body ...)
     #'(:p attrs.group ... body.compiled ...)]

    ;; TODO: Remove refresh-every from interpreter and move it all here.
    [(refresh-every n:number)
     #'(interpret '(refresh-every n) *env*)]

    [(script ~! content:string ...+)
     #'(:script (string-append content ...))]

    [(span ~! attrs:attrs body:body ...+)
     #'(:span attrs.group ... body.compiled ...)]

    [(strong ~! attrs:attrs body:body ...+)
     #'(:strong attrs.group ... body.compiled ...)]

    [(style ~! content:string ...+)
     #'(:style
        ([:type "text/css"])
        (string-append content ...))]

    [(ol ~! attrs:attrs item:list-item ...+)
     #'(:ol attrs.group ... item.xexpr ... ...)]

    [(ul ~! attrs:attrs item:list-item ...+)
     #'(:ul attrs.group ... item.xexpr ... ...)]

    [(template id:id content ...+)
     (define template-id (syntax-e #'id))
     (unless (hash-has-key? (Env-templates (current-env)) template-id)
       (raise-syntax-error 'template (format "~a is not a known template" template-id) stx #'id))
     (define template-type
       (hash-ref (Env-templates (current-env)) template-id))
     (define content-stxes
       (case template-type
         [(ungrouped) (syntax-e #'(content ...))]
         [(grouped) (syntax-e (group-by-paragraph #'(content ...)))]
         [else (raise-syntax-error 'template (format "unexpected type ~a for template ~a" template-type template-id))]))
     (with-syntax ([(compiled-content ...) (map compile-markup content-stxes)])
       #`(id (λ () #,(if (= (length (syntax-e #'(compiled-content ...))) 1)
                         #'(list (haml compiled-content ...))
                         #'(haml compiled-content ...)))))]

    [(template ~! id:id)
     #'(id (λ () (error 'template "yielded without content")))]

    [(yield ~!)
     (unless (current-in-template?)
       (raise-syntax-error 'yield "cannot yield outside template" stx stx))
     #'(unquote-splicing (content-proc))]

    [_ (compile-expr stx)]))

(define (compile-expr stx)
  (syntax-parse stx
    #:datum-literals (ev splicing-ev)
    [(ev e) #'(interpret 'e *env*)]
    [(splicing-ev e) #'(unquote-splicing (interpret 'e *env*))]))

(define (compile-action-expr stx)
  (syntax-parse stx
    [{~or " " "\n"} #f]
    [_ (compile-expr stx)]))

(define (compile-form-expr stx)
  (define (stx->keyword-stx stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

  (syntax-parse stx
    #:datum-literals (checkbox input-date input-file input-number input-range input-text input-time radios select textarea submit-button)
    [({~and {~or checkbox input-date input-file input-number input-range input-text input-time textarea} widget-id} name:id label-expr)
     #:with name-kwd (stx->keyword-stx #'name)
     #:with compiled-label-expr (compile-markup (syntax-e #'label-expr))
     #'((name-kwd
         (widget-id
          (haml compiled-label-expr))))]

    [({~and {~or checkbox input-date input-file input-number input-range input-text input-time textarea} widget-id}
      name:id
      {~optional {~seq #:required? required?:boolean}}
      label-expr)
     #:with name-kwd (stx->keyword-stx #'name)
     #:with compiled-label-expr (compile-markup (syntax-e #'label-expr))
     #'((name-kwd
         (widget-id
          {~? {~@ #:required? required?}}
          (haml compiled-label-expr))))]

    [({~and {~or input-number input-range} widget-id} name:id
                   ~!
                   {~alt
                    {~optional {~seq #:required? required?:boolean}}
                    {~optional {~seq #:min min-expr:number}}
                    {~optional {~seq #:max max-expr:number}}
                    {~optional {~seq #:step step-expr:number}}} ...
                   label-expr)
     #:with name-kwd (stx->keyword-stx #'name)
     #:with compiled-label-expr (compile-markup (syntax-e #'label-expr))
     #'((name-kwd
         (widget-id
          {~? {~@ #:required? required?}}
          {~? {~@ #:min min-expr}}
          {~? {~@ #:max max-expr}}
          {~? {~@ #:step step-expr}}
          (haml compiled-label-expr))))]

    [({~and {~or radios select} widget-id} name:id
             options
             {~optional {~seq #:required? required?:boolean}}
             label-expr)
     #:with name-kwd (stx->keyword-stx #'name)
     #:with compiled-label-expr (compile-markup (syntax-e #'label-expr))
     #'((name-kwd
         (widget-id
          {~? {~@ #:required? required?}}
          (haml compiled-label-expr)
          options)))]

    ; FIXME: The next clause does not work with #:required
    [({~and {~or checkbox input-date input-file input-number input-range input-text input-time textarea} widget-id} ~! name:id label-expr ...+)
     (compile-form-expr #'(widget-id name (div label-expr ...)))]

    [(submit-button {~optional label:string})
     #'((:button.button.next-button ([:type "submit"]) {~? label "Submit"}))]

    [e
     #:with compiled-markup (compile-markup #'e)
     #'(compiled-markup)]))

(define (check-action-id who stx)
  (unless (or (not (syntax-e stx))
              (memv (syntax->datum stx)
                    (Env-actions (current-env))))
    (raise-syntax-error who (format "~a is not a known action" (syntax-e stx)) stx)))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-paragraph-tags
  (make-parameter '(a br em ev span strong)))

(define (swallow? stx)
  (member (syntax-e (car (syntax-e stx))) (current-paragraph-tags)))

;; invariant: never two strings next to each other where one isn't "\n"
(define (group-by-paragraph stx)
  ;; TODO: make it so swallowed things can't have newlines in them
  (let loop ([stxs (syntax-e stx)]
             [pending null]
             [res null])

    ; "hello" (:h1 "title") "there"
    (cond
      [(null? stxs)
       (datum->syntax stx (append res (expand-paragraphs (reverse pending))))]
      [else
       (define stx (car stxs))
       (define stx-str? (string? (syntax-e stx)))
       (cond
         ;; not a string but can be swallowed (eg @strong)
         [(and (not stx-str?) (swallow? stx))
          (loop (cdr stxs) (cons stx pending) res)]

         ;; not a string and can't be swallowed
         [(not stx-str?)
          (loop (cdr stxs) null (append res
                                        (expand-paragraphs (reverse pending))
                                        (list stx)))]

         ;; a string
         [else
          (loop (cdr stxs) (cons stx pending) res)])])))

(define (expand-paragraphs pending)
  (let loop ([pending pending]
             [depth 0]
             [paras null])
    (cond
      [(null? pending)
       (reverse paras)]
      [else
       (define stx
         (car pending))
       (define-values (new-depth new-paras)
         (cond
           [(equal? (syntax-e stx) "\n")
            (values (add1 depth) paras)]

           [(> depth 1)
            (values 0 (cons
                       (quasisyntax/loc stx
                         (p #,stx))
                       paras))]

           [(and (= depth 1) (null? paras))
            (values 0 (list (quasisyntax/loc stx
                              (p #,stx))))]

           [(= depth 1)
            (values 0 (cons
                       (quasisyntax/loc (car paras)
                         (#,@(car paras) " " #,stx))
                       (cdr paras)))]

           [(null? paras)
            (values 0 (cons (quasisyntax/loc stx
                              (p #,stx))
                            paras))]

           [else
            (values 0 (cons (quasisyntax/loc (car paras)
                              (#,@(car paras) #,stx))
                            (cdr paras)))]))
       (loop (cdr pending) new-depth new-paras)])))

(module+ test
  (require rackunit)

  (check-equal? (syntax->datum (group-by-paragraph #'())) '())
  (check-equal? (syntax->datum (group-by-paragraph #'("hello")))
                '((p "hello")))
  (check-equal? (syntax->datum (group-by-paragraph #'((h1 "Title") "hello" "\n" "\n" "friend")))
                '((h1 "Title")
                  (p "hello")
                  (p "friend")))

  (check-equal? (syntax->datum (group-by-paragraph #'("hello" "\n" "there")))
                '((p "hello" " " "there")))
  (check-equal? (syntax->datum (group-by-paragraph #'("hello" "\n" "\n" "there")))
                '((p "hello") (p "there")))
  (check-equal? (syntax->datum (group-by-paragraph #'((h1 "Page title") "hello" "\n" "there" "\n" "\n" "\n" "friend")))
                '((h1 "Page title")
                  (p "hello" " " "there")
                  (p "friend")))
  (check-equal? (syntax->datum (group-by-paragraph #'("a" "\n" "b" "\n" "\n" "c" "\n" (h1 "d"))))
                '((p "a" " " "b")
                  (p "c")
                  (h1 "d"))))
