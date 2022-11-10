#lang racket/base

(require (for-syntax racket/base)
         koyo/haml
         racket/list
         racket/string
         racket/syntax
         (prefix-in s: scribble/reader)
         syntax/parse

         "dsl-runtime.rkt"
         "formular.rkt"
         "registry.rkt"
         "study.rkt"
         "transition-graph.rkt")

(provide
 dsl-require
 read-syntax+compile)

(define attached-mods
  '(congame/components/dsl-runtime
    congame/components/formular
    congame/components/registry
    congame/components/study
    congame/components/transition-graph
    koyo/haml))

;; Next time:
;;  * maybe add caching?
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
    (for-each eval (syntax->datum (compile-module (read-syntax 'dsl in))))
    (namespace-variable-value id)))

(define (read-syntax what in)
  (s:read-syntax-inside what in))

(define (read-syntax+compile what in)
  (compile-module (read-syntax what in)))


;; compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-literals set-id [id ...])
  (begin
    (define-syntax (id stx)
      (raise-syntax-error 'id "may only be usde inside DSL" stx)) ...
    (define-literal-set set-id
      (id ...))))

(define-literals dsl-literals
  [:p :br])

(struct Env (actions imports templates)
  #:transparent)

(define current-env
  (make-parameter #f))

(define (build-env stxs)
  (define-values (actions imports templates)
    (for/fold ([actions null]
               [imports null]
               [templates (hash)])
              ([stx (in-list stxs)])
      (syntax-parse stx
        #:datum-literals (action import template template-ungrouped)
        [(action id:id _ ...)
         (values (cons (syntax->datum #'id) actions) imports templates)]
        [(import _ id:id)
         (values actions (cons (syntax->datum #'id) imports) templates)]
        [(template id:id _ ...)
         (values actions imports (hash-set templates (syntax->datum #'id) 'grouped))]
        [(template-ungrouped id:id _ ...)
         (values actions imports (hash-set templates (syntax->datum #'id) 'ungrouped))]
        [_
         (values actions imports templates)])))
  (Env actions imports templates))

(define (compile-module stx)
  (syntax-parse stx
    [(statement ...)
     (parameterize ([current-env (build-env (syntax-e #'(statement ...)))])
       (with-syntax ([(compiled-stmt ...) (filter-map compile-stmt (syntax-e #'(statement ...)))])
         #'(compiled-stmt ...)))]))

(define (compile-stmt stx)
  (define-syntax-class cond-expr
    #:datum-literals (cond else)
    (pattern (cond [clause-expr clause-id:id] ...+
                   [else else-id:id])
             #:attr id #f
             #:with (compiled-clause-expr ...) (map compile-cond-expr (syntax-e #'(clause-expr ...)))
             #:with compiled #'(cond
                                 [compiled-clause-expr (goto clause-id)] ...
                                 [else (goto else-id)])))

  (define-syntax-class transition-entry
    #:datum-literals (--> cond else lambda)
    (pattern -->
             #:attr id #f
             #:with compiled this-syntax)

    (pattern id:id
             #:with compiled this-syntax)

    (pattern {~and (cond _ ...) e:cond-expr}
             #:attr id #f
             #:with compiled #'(unquote (λ () e.compiled)))

    (pattern (lambda action-expr ... target-id:id)
             #:attr id #f
             #:with (compiled-action-expr ...) (map compile-action-expr (syntax-e #'(action-expr ...)))
             #:with compiled #'(unquote
                                (λ ()
                                  compiled-action-expr ...
                                  (goto target-id))))

    (pattern (lambda action-expr ... {~and (cond _ ...) cond-expr:cond-expr})
             #:attr id #f
             #:with (compiled-action-expr ...) (map compile-action-expr (syntax-e #'(action-expr ...)))
             #:with compiled #'(unquote
                                (λ ()
                                  compiled-action-expr ...
                                  cond-expr.compiled))))

  (syntax-parse stx
    #:datum-literals (action import step study template template-ungrouped)
    [{~or " " "\n"} #f]

    [(action name:id content ...+)
     #:with (compiled-content ...) (map compile-action-expr (syntax-e #'(content ...)))
     #'(define (name)
         compiled-content ...)]

    [(import mod-path:id id:id)
     #'(define id (study-mod-require 'mod-path 'id))]

    [(step name:id content ...+)
     #:with (compiled-content ...) (map compile-expr (syntax-e (group-by-paragraph #'(content ...))))
     #'(define (name)
         (page
          (haml
           (.container
            compiled-content ...))))]

    [(study study-id:id #:transitions [transition-entry:transition-entry ...] ...+)
     #:with study-id-str (datum->syntax #'study-id (symbol->string (syntax->datum #'study-id)))
     #:with (step-id ...) (for*/fold ([stxes null]
                                      [seen-ids (hasheq)]
                                      #:result (reverse stxes))
                                     ([id-stx (in-list (syntax-e #'({~? transition-entry.id} ... ...)))]
                                      [id (in-value (syntax->datum id-stx))]
                                      #:unless (hash-has-key? seen-ids id))
                            (values (cons id-stx stxes)
                                    (hash-set seen-ids id #t)))
     #'(define study-id
         (make-study
          study-id-str
          #:transitions (transition-graph [transition-entry.compiled ...] ...)
          (list
           (if (study? step-id)
               (make-step/study 'step-id step-id)
               (make-step 'step-id step-id)) ...)))]

    [({~and {~or template template-ungrouped} form-id} template-id:id content ...+)
     #:with (compiled-content ...) (parameterize ([current-paragraph-tags
                                                   (if (eq? (syntax-e #'form-id) 'template-ungrouped)
                                                       (cons 'yield (current-paragraph-tags))
                                                       (current-paragraph-tags))])
                                     (map compile-expr (syntax-e (group-by-paragraph #'(content ...)))))
     #'(define (template-id content-proc)
         (haml (:div compiled-content ...)))]))

(define (compile-expr stx)
  (define-syntax-class body
    (pattern body #:with compiled (compile-expr #'body)))

  (define-syntax-class list-item
    #:datum-literals (li)
    #:attributes ([xexpr 1])
    (pattern "\n"
             #:with (xexpr ...) null)
    (pattern (li body:body ...+)
             #:with (xexpr ...) #'((:li body.compiled ...))))

  ;; NOTE: For block-style tags, it's the tag's responibility to call
  ;; group-by-paragraph on its exprs.
  (syntax-parse stx
    #:datum-literals (a button call div em form h1 h2 h3 img template span strong ol ul yield)
    #:literal-sets (dsl-literals)
    [str:string #'str]

    [(a url:string body:body ...+)
     #'(:a ([:href url]) body.compiled ...)]

    [(button {~optional {~seq #:action action:id}} text0:string text:string ...)
     #:with joined-text (datum->syntax #'text0 (string-join (syntax->datum #'(text0 text ...)) ""))
     (check-action-id 'button #'{~? action #f})
     #'(button {~? action void} joined-text)]

    [(call _id:id _e ...)
     (compile-call-expr stx)]

    [(div {~optional {~seq #:class class:string}} body ...+)
     #:with (compiled-body ...) (map compile-expr (syntax-e (group-by-paragraph #'(body ...))))
     #'(:div {~? ({~@ [:class class]})} compiled-body ...)]

    [(em body:body ...+)
     #'(:em body.compiled ...)]

    [(form {~optional {~seq #:action action:id}} body ...+)
     #:with ((compiled-body ...) ...) (map compile-form-expr (syntax-e (group-by-paragraph #'(body ...))))
     (check-action-id 'form #'{~? action #f})
     #'(formular
        (haml
         (:div
          compiled-body ... ...))
        (make-put-all-keywords {~? action void}))]

    [({~and {~or h1 h2 h3} tag} text0 text ...)
     #:with tag-id (format-id #'tag ":~a" #'tag)
     (unless (string? (syntax-e #'text0))
       (raise-syntax-error 'tag "expected text" stx))
     #'(tag-id text0 text ...)]

    [(img url:string)
     #'(:img ([:href url]))]

    [(span {~optional {~seq #:class class:string}} body:body ...+)
     #'(:span {~? ({~@ [:class class]})} body.compiled ...)]

    [(strong body:body ...+)
     #'(:strong body.compiled ...)]

    [(ol item:list-item ...+)
     #'(:ol item.xexpr ... ...)]

    [(ul item:list-item ...+)
     #'(:ul item.xexpr ... ...)]

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
     (with-syntax ([(compiled-content ...) (map compile-expr content-stxes)])
       #`(id (λ () #,(if (= (length (syntax-e #'(compiled-content ...))) 1)
                         #'(list (haml compiled-content ...))
                         #'(haml compiled-content ...)))))]

    [(template id:id)
     #'(id (λ () (error 'template "yielded without content")))]

    [(yield) ;; TODO: check that we're in a template
     #'(unquote-splicing (content-proc))]

    [(:p body:body ...)
     #'(:p body.compiled ...)]

    [(:br)
     #'(:br)]))

(define (compile-call-expr stx)
  (syntax-parse stx
    [(_ id:id e ...)
     (define the-id (syntax->datum #'id))
     (unless (member the-id (Env-imports (current-env)))
       (raise-syntax-error 'call (format "unknown procedure ~a; did you forget to import it?" the-id) stx))
     #'(id e ...)]))

(define (compile-action-expr stx)
  (syntax-parse stx
    #:datum-literals (call)
    [(call _id:id _e ...)
     (compile-call-expr stx)]))

(define (compile-cond-expr stx)
  (syntax-parse stx
    #:datum-literals (= get)
    [num:number #'num]
    [str:string #'str]
    [(= e0 e1)
     #:with compiled-e0 (compile-cond-expr #'e0)
     #:with compiled-e1 (compile-cond-expr #'e1)
     #'(equal? compiled-e0 compiled-e1)]
    [(get id:id)
     #'(get 'id)]))

(define (compile-form-expr stx)
  (define (stx->keyword-stx stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

  (syntax-parse stx
    #:datum-literals (input-date input-text input-number textarea submit-button)
    [({~and {~or input-date input-text input-number textarea} widget-id} name:id label-expr)
     #:with name-kwd (stx->keyword-stx #'name)
     #:with compiled-label-expr (compile-expr (syntax-e #'label-expr))
     #'((name-kwd
         (widget-id
          (haml compiled-label-expr))))]

    [(input-number name:id
                   ~!
                   {~alt
                    {~optional {~seq #:min min-expr:number}}
                    {~optional {~seq #:max max-expr:number}}} ...
                   label-expr)
     #:with name-kwd (stx->keyword-stx #'name)
     #:with compiled-label-expr (compile-expr (syntax-e #'label-expr))
     #'((name-kwd
         (input-number
          {~? {~@ #:min min-expr}}
          {~? {~@ #:max max-expr}}
          (haml compiled-label-expr))))]

    [({~and {~or input-date input-text input-number textarea} widget-id} ~! name:id label-expr ...+)
     (compile-form-expr #'(widget-id name (:div label-expr ...)))]

    [(submit-button)
     #'((:button.button.next-button ([:type "submit"]) "Submit"))]

    [e
     #:with compiled-expr (compile-expr #'e)
     #'(compiled-expr)]))

(define (check-action-id who stx)
  (unless (or (not (syntax-e stx))
              (memv (syntax->datum stx)
                    (Env-actions (current-env))))
    (raise-syntax-error who (format "~a is not a known action" (syntax-e stx)) stx)))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-paragraph-tags
  (make-parameter '(a em span strong)))

;; invariant: never two strings next to each other where one isn't "\n"
(define (group-by-paragraph stx)
  (define (swallow? stx)
    (member (syntax-e (car (syntax-e stx)))
            (current-paragraph-tags)))

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
                         (:p #,stx))
                       paras))]

           [(and (= depth 1) (null? paras))
            (values 0 (list (quasisyntax/loc stx
                              (:p #,stx))))]

           [(= depth 1)
            (values 0 (cons
                       (quasisyntax/loc (car paras)
                         (#,@(car paras) (:br) #,stx))
                       (cdr paras)))]

           [(null? paras)
            (values 0 (cons (quasisyntax/loc stx
                              (:p #,stx))
                            paras))]

           [else
            (values 0 (cons (quasisyntax/loc (car paras)
                              (#,@(car paras) #,stx))
                            (cdr paras)))]))
       (loop (cdr pending) new-depth new-paras)])))

(module+ test
  (require rackunit)

  (define (read+compile str)
    (define in (open-input-string str))
    (port-count-lines! in)
    (compile-module (read-syntax "<string>" in)))

  (check-equal? (syntax->datum (group-by-paragraph #'())) '())
  (check-equal? (syntax->datum (group-by-paragraph #'("hello")))
                '((:p "hello")))
  (check-equal? (syntax->datum (group-by-paragraph #'((:h1 "Title") "hello" "\n" "\n" "friend")))
                '((:h1 "Title")
                  (:p "hello")
                  (:p "friend")))

  (check-equal? (syntax->datum (group-by-paragraph #'("hello" "\n" "there")))
                '((:p "hello" (:br) "there")))
  (check-equal? (syntax->datum (group-by-paragraph #'("hello" "\n" "\n" "there")))
                '((:p "hello") (:p "there")))
  (check-equal? (syntax->datum (group-by-paragraph #'((:h1 "Page title") "hello" "\n" "there" "\n" "\n" "\n" "friend")))
                '((:h1 "Page title")
                  (:p "hello" (:br) "there")
                  (:p "friend")))
  (check-equal? (syntax->datum (group-by-paragraph #'("a" "\n" "b" "\n" "\n" "c" "\n" (:h1 "d"))))
                '((:p "a" (:br) "b")
                  (:p "c")
                  (:h1 "d")))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[a]{
  a
  b

  c
}
DSL
                  ))
   '((define (a)
       (page
        (haml
         (.container
          (:p "a" (:br) "b")
          (:p "c")))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[a]{
  a
  b

  c

  @h1{Heading}
}
DSL
                  ))
   '((define (a)
       (page
        (haml
         (.container
          (:p "a" (:br) "b")
          (:p "c")
          (:h1 "Heading")))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[a]{
  a
  b

  c

  @h1{Heading}

  d
  e

  f
}
DSL
                  ))
   '((define (a)
       (page
        (haml
         (.container
          (:p "a" (:br) "b")
          (:p "c")
          (:h1 "Heading")
          (:p "d" (:br) "e")
          (:p "f")))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[a]{
  @h1{Hello, world!}
  How's it going?
  Pretty good?

  Yeah, good.
}
DSL
                  ))
   '((define (a)
       (page
        (haml
         (.container
          (:h1 "Hello, world!")
          (:p "How's it going?" (:br) "Pretty good?")
          (:p "Yeah, good.")))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[hello]{
  @h1{Hello!}
  @button{Continue...}
}

@step[done]{
  You're done!
}

@study[
  hello-study
  #:transitions
  [hello --> done]
  [done --> done]
]
DSL
                  ))
   '((define (hello)
       (page
        (haml
         (.container
          (:h1 "Hello!")
          (button void "Continue...")))))
     (define (done)
       (page
        (haml
         (.container
          (:p "You're done!")))))
     (define hello-study
       (make-study
        "hello-study"
        #:transitions (transition-graph
                       [hello --> done]
                       [done --> done])
        (list
         (if (study? hello)
             (make-step/study 'hello hello)
             (make-step 'hello hello))
         (if (study? done)
             (make-step/study 'done done)
             (make-step 'done done)))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@import[a b]
DSL
                  ))
   '((define b (study-mod-require 'a 'b))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[form-step]{
  @form{
    @h1{Section 1}
    @input-text[name]{What is your name?}
    @h1{Section 2}
    @input-number[age #:max 100 #:min 1]{How old are you?}
    @submit-button[]
  }
}
DSL
                  ))
   '((define (form-step)
       (page
        (haml
         (.container
          (formular
           (haml
            (:div
             (:h1 "Section 1")
             (#:name
              (input-text
               (haml "What is your name?")))
             (:h1 "Section 2")
             (#:age
              (input-number
               #:min 1
               #:max 100
               (haml "How old are you?")))
             (:button.button.next-button
              ([:type "submit"])
              "Submit")))
           (make-put-all-keywords void))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @ol{
    @li{Test}
    @li{Foo}
  }
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:ol
           (:li "Test")
           (:li "Foo"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @ul{
    @li{Test}
    @li{Foo}
  }
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:ul
           (:li "Test")
           (:li "Foo"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @ul{
    @li{
      @ul{
        @li{Nested 1}
        @li{Nested 2}
      }
    }
    @li{Foo}
  }
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:ul
           (:li
            (:ul
             (:li "Nested 1")
             (:li "Nested 2")))
           (:li "Foo"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @a["http://example.com"]{example.com}
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:p (:a ([:href "http://example.com"]) "example.com"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @strong{Hello @em{world}!}
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:p
           (:strong "Hello " (:em "world") "!"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  Hello @strong{world}!
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:p
           "Hello "
           (:strong "world")
           "!")))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @div{Hello @em{world}!}
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:div (:p "Hello " (:em "world") "!"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @div{
   Hello

   @em{world}!
  }
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:div
           (:p "Hello")
           (:p (:em "world") "!"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[example-step]{
  @div[#:class "example"]{
   Hello

   @em{world}!
  }
}
DSL
                  ))
   '((define (example-step)
       (page
        (haml
         (.container
          (:div
           ([:class "example"])
           (:p "Hello")
           (:p (:em "world") "!"))))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@step[step0]{@button{Continue}}
@step[step1]{@button{Continue}}
@step[step2]{@button{Continue}}
@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [step0 --> @cond[[@=[@get[some-var] "agree"] step1]
                   [@else step2]]]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
DSL
                  ))
   '((define (step0) (page (haml (.container (button void "Continue")))))
     (define (step1) (page (haml (.container (button void "Continue")))))
     (define (step2) (page (haml (.container (button void "Continue")))))
     (define (done) (page (haml (.container (:p "You're done.")))))
     (define hello-study
       (make-study
        "hello-study"
        #:transitions (transition-graph
                       [step0 --> ,(λ ()
                                     (cond
                                       [(equal? (get 'some-var) "agree")
                                        (goto step1)]
                                       [else
                                        (goto step2)]))]
                       [step1 --> done]
                       [step2 --> done]
                       [done --> done])
        (list
         (if (study? step0)
             (make-step/study 'step0 step0)
             (make-step 'step0 step0))
         (if (study? step1)
             (make-step/study 'step1 step1)
             (make-step 'step1 step1))
         (if (study? done)
             (make-step/study 'done done)
             (make-step 'done done))
         (if (study? step2)
             (make-step/study 'step2 step2)
             (make-step 'step2 step2)))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@import[racket/base println]

@action[quit]{
  @call[println "hello"]
}

@step[hello]{
  @button[#:action quit]{Quit}
}
DSL
                  ))
   '((define println (study-mod-require 'racket/base 'println))
     (define (quit) (println "hello"))
     (define (hello) (page (haml (.container (button quit "Quit")))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@import[racket/base println]

@step[step0]{@button{Continue}}
@step[step1]{@button{Continue}}
@step[step2]{@button{Continue}}
@step[done]{You're done.}

@study[
  hello-study
  #:transitions
  [step0 --> @lambda[
               @call[println "hello"]
               @cond[[@=[@get[some-var] "agree"] step1]
                     [@else step2]]]]
  [step1 --> done]
  [step2 --> done]
  [done --> done]
]
DSL
                  ))
   '((define println (study-mod-require 'racket/base 'println))
     (define (step0) (page (haml (.container (button void "Continue")))))
     (define (step1) (page (haml (.container (button void "Continue")))))
     (define (step2) (page (haml (.container (button void "Continue")))))
     (define (done) (page (haml (.container (:p "You're done.")))))
     (define hello-study
       (make-study
        "hello-study"
        #:transitions (transition-graph
                       [step0 --> ,(λ ()
                                     (println "hello")
                                     (cond
                                       [(equal? (get 'some-var) "agree")
                                        (goto step1)]
                                       [else
                                        (goto step2)]))]
                       [step1 --> done]
                       [step2 --> done]
                       [done --> done])
        (list
         (if (study? step0)
             (make-step/study 'step0 step0)
             (make-step 'step0 step0))
         (if (study? step1)
             (make-step/study 'step1 step1)
             (make-step 'step1 step1))
         (if (study? done)
             (make-step/study 'done done)
             (make-step 'done done))
         (if (study? step2)
             (make-step/study 'step2 step2)
             (make-step 'step2 step2)))))))

  (check-equal?
   (syntax->datum
    (read+compile #<<DSL
@template-ungrouped[a]{
  Hello @yield[]!
}

@template[b]{
  @template[a]{
    Hi world!
  }

  @template[a]{
    Hello

    there

    @h1{Friend}
  }
}

@step[hello]{
  @template[b]
}
DSL
                  ))
   '((define (a content-proc)
       (haml
        (:div
         (:p "Hello " ,@(content-proc) "!"))))
     (define (b content-proc)
       (haml
        (:div
         (a (λ () (list (haml "Hi world!"))))
         (a (λ () (haml "Hello" "\n" "\n" "there" "\n" "\n" (:h1 "Friend")))))))
     (define (hello)
       (page
        (haml
         (.container
          (b (λ () (error 'template "yielded without content"))))))))))
