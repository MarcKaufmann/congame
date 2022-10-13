#lang racket/base

(require (for-syntax racket/base)
         koyo/haml
         racket/list
         racket/string
         racket/syntax
         (prefix-in s: scribble/reader)
         syntax/parse

         "study.rkt"
         "transition-graph.rkt")

(provide
 dsl-require)

(define attached-mods
  '((congame/components/study
     congame/componetns/transition-graph
     koyo/yaml)))

;; Next time:
;;  * add support for @import
;;  * add support for @form[]s
(define (dsl-require s id)
  (define in (open-input-string s))
  (port-count-lines! in)
  (define ns (current-namespace))
  (parameterize ([current-namespace (make-base-namespace)])
    (for ([mod (in-list attached-mods)])
      (namespace-attach-module ns mod))
    (for ([mod (in-list attached-mods)])
      (eval `(require ,mod)))
    (for-each eval (syntax->datum (compile-module (read-syntax 'dsl in))))
    (namespace-variable-value id)))

(define (read-syntax what in)
  (s:read-syntax-inside what in))


;; compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-literals set-id [id ...])
  (begin
    (define-syntax (id stx)
      (raise-syntax-error 'id "may only be usde inside DSL" stx)) ...
    (define-literal-set set-id
      (id ...))))

(define-literals dsl-literals
  [:p :br])

(define (compile-module stx)
  (syntax-parse stx
    [(statement ...)
     (with-syntax ([(compiled-stmt ...) (filter-map compile-stmt (syntax-e #'(statement ...)))])
       #'(compiled-stmt ...))]))

(define (compile-stmt stx)
  (syntax-parse stx
    #:datum-literals (step study)
    [{~or " " "\n"} #f]
    [(step name:id content ...+)
     (with-syntax ([(compiled-content ...) (map compile-expr (syntax-e (group-by-paragraph #'(content ...))))])
       #'(define (name)
           (page
            (haml
             (.container
              compiled-content ...)))))]
    [(study study-id:id #:transitions [transition-entry:id ...] ...+)
     #:with study-id-str (datum->syntax #'study-id (symbol->string (syntax->datum #'study-id)))
     #:with (step-id ...) (for*/fold ([stxes null]
                                      [seen-ids (hasheq)]
                                      #:result (reverse stxes))
                                     ([id-stx (in-list (syntax-e #'(transition-entry ... ...)))]
                                      [id (in-value (syntax->datum id-stx))]
                                      #:unless (eq? (syntax-e id-stx) '-->)
                                      #:unless (hash-has-key? seen-ids id))
                            (values (cons id-stx stxes)
                                    (hash-set seen-ids id #t)))
     #'(define study-id
         (make-study
          study-id-str
          #:transitions (transition-graph [transition-entry ...] ...)
          (list
           (make-step 'step-id step-id) ...)))]))

(define (compile-expr stx)
  (syntax-parse stx
    #:datum-literals (button h1 h2 h3)
    #:literal-sets (dsl-literals)
    [(button text0:string text:string ...)
     #:with joined-text (datum->syntax #'text0 (string-join (syntax->datum #'(text0 text ...)) ""))
     #'(button void joined-text)]

    [({~and {~or h1 h2 h3} tag} text0 text ...)
     #:with tag-id (format-id #'tag ":~a" #'tag)
     (unless (string? (syntax-e #'text0))
       (raise-syntax-error 'tag "expected text" stx))
     #'(tag-id text0 text ...)]

    [(:p body ...)
     #:with (compiled-body ...) (map compile-expr (syntax-e #'(body ...)))
     #'(:p compiled-body ...)]

    [(:br)
     #'(:br)]

    [(rator rand ...)
     (raise-syntax-error 'dsl "invalid expression" stx)]

    [e
     (datum->syntax #'here (syntax->datum #'e))]))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; invariant: never two strings next to each other where one isn't "\n"
(define (group-by-paragraph stx)
  (define (swallow? stx)
    (member (syntax-e (car (syntax-e stx))) '(bold em)))

  ;; TODO: make it so swallowed things can't have newlines in them
  (let loop ([stxs (syntax-e stx)]
             [pending null]
             [res null])

    ; "hello" (:h1 "title") "there"
    (cond
      [(null? stxs)
       (datum->syntax stx (append (reverse res) (expand-paragraphs (reverse pending))))]
      [else
       (define stx (car stxs))
       (define stx-str? (string? (syntax-e stx)))
       (cond
         ;; not a string but can be swalloed (eg @bold)
         [(and (not stx-str?) (swallow? stx))
          (loop (cdr stxs) (cons stx pending) res)]

         ;; not a string and can't be swallowed
         [(not stx-str?)
          (loop (cdr stxs) null (cons stx (append (expand-paragraphs (reverse pending)) res)))]

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

  (check-equal?
   '((define (a)
       (page
        (haml
         (.container
          (:h1 "Hello, world!")
          (:p "How's it going?" (:br) "Pretty good?")
          (:p "Yeah, good."))))))
   (syntax->datum
    (read+compile #<<DSL
@step[a]{
  @h1{Hello, world!}
  How's it going?
  Pretty good?

  Yeah, good.
}
DSL
                  )))

  (check-equal?
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
         (make-step 'hello hello)
         (make-step 'done done)))))
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
))))
