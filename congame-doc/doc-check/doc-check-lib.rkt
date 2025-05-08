#lang racket/base

(require racket/list
         racket/match
         racket/string
         racket/symbol
         threading)

(provide (all-defined-out))

;;================================================
;; Binding info

(struct binding (id module type) #:transparent)
(struct test (module count missing-bindings) #:transparent)

;;================================================
;; Parameters

(define modules-to-check (make-parameter '()))
(define ignored-bindings (make-parameter '()))  ; Discard bindings from these modules

;;================================================
;; Get bindings

;; "module.rkt" 'conscript/base -> 'conscript/module
(define (->modpath v in-mod)
  (cond
    [(not (string? v)) v]
    [else
     (define modbase
       (~> (symbol->immutable-string in-mod)
           (string-split "/")
           car))
     (~> (path-replace-extension v #"")
         (format "~a/~a" modbase _)
         string->symbol)]))

;; Returns a list of bindings containing the original identifier and module name, as well as
;; type 'proc or 'form
(define (get-bindings mod #:type t)
  (dynamic-require mod #f)
  (define-values (vars forms) (module->exports mod))
  (define lst (if (eq? t 'proc) vars forms))
  (cond
    [(null? lst) lst]
    [else
     (for/list ([export (in-list (cdar lst))]) ; cdar = only use phase 0
       (match export
         [(list id (? null?))
          (binding id mod t)]
         [(list id (list (list (? module-path-index?) _ orig-id _) (? module-path-index? orig-mp-idx)))
          (define-values (orig-mod _base) (module-path-index-split orig-mp-idx))
          (binding orig-id orig-mod t)]
         [(list id (list (list (? module-path-index? mp-idx) _ orig-id _)))
          (define-values (orig-mod _base) (module-path-index-split mp-idx))
          (binding orig-id orig-mod t)]
         [(list id (list (? module-path-index? mp-idx)))
          (define-values (orig-mod _base) (module-path-index-split mp-idx))
          (binding id (->modpath orig-mod mod) t)]))]))

(define (get-all-bindings mod)
  (append (get-bindings mod #:type 'proc)
          (get-bindings mod #:type 'form)))

(define (ignore-binding? b)
  (match-define (binding id mod _) b)
  (or
   (match mod
     [(? symbol? m)
      (~> (symbol->immutable-string m)
          (string-prefix? _ "racket/"))]
     [_ #f])
   (for/or ([ignore-pat (in-list (ignored-bindings))])
     (match ignore-pat
       [(? symbol? ignore-mod)
        (eq? mod ignore-mod)]
       [(list ignore-mod ids-to-ignore ...)
        (and (eq? mod ignore-mod)
             (member id ids-to-ignore))]))))

(define (binding-in-mod? b m)
  (eq? (binding-module b) m))

(define (all-bindings/filter)
  (remove-duplicates
   (flatten
    (for/list ([mod (in-list (modules-to-check))])
      (filter-not ignore-binding? (get-all-bindings mod))))))

;;================================================
;; Modules

(define (bindings-list-mod<? lst-a lst-b)
  (define a (binding-module (car lst-a)))
  (define b (binding-module (car lst-b)))
  (cond
    [(andmap symbol? (list a b))
     (symbol<? a b)]
    [(list? a) #f]
    [else #t]))

(define (group-by-module bindings)
  (sort (group-by binding-module bindings) bindings-list-mod<?))

;;================================================
;; Checking Scribble docs

(require racket/format
         racket/promise
         scribble/xref
         setup/xref)

(define collections-xrefs (delay (load-collections-xref)))

;; binding? -> boolean?
(define (documented? b)
  (and (xref-binding->definition-tag (force collections-xrefs)
                                     (list (binding-module b) (binding-id b)) #f) #t))

(define (scribble-module modname)
  (~a "@;================================================\n\n"
      "@defmodule[" modname "]\n\n"))

(define (stub-scribble-def bdg)
  (match-define (binding id _mod type) bdg)
  (case type
    [(proc)
     (~a "@defproc[(" id " [arg any/c]) any/c]{\n\n"
         id " proc\n\n"
         "}\n\n")]
    [(form)
     (~a "@defform[(" id " arg)\n"
         "         #:contracts ([arg any/c])]{\n\n"
         id " form\n\n"
         "}\n\n")]))


;;================================================
;; Fancy console output

(define in-term?
  (delay (~> (find-system-path 'exec-file)
             path->string
             string-downcase
             (regexp-match? #rx"gracket" _)
             not)))

(define (term-bytes . b)
  (when (force in-term?) (for-each write-bytes b)))

;; From https://github.com/lexi-lambda/racket-commonmark/blob/master/commonmark-test/tests/commonmark/spec.rkt

(define color:bold #"\e[1m")
(define color:red #"\e[31m")
(define color:green #"\e[32m")
(define color:yellow #"\e[33m")
(define color:gray #"\e[90m")
(define color:reset #"\e(B\e[m")

(define section-title-width (make-parameter 10))
(define totals-width (make-parameter 10))

(define (write-chars c len)
  (for ([i (in-range len)])
    (write-char c)))

(define (write-separator)
  (write-chars #\─ (+ (section-title-width) (* (totals-width) 2) 58))
  (newline))

(define (write-header str)
  (newline)
  (term-bytes color:bold)
  (write-string str)
  (term-bytes color:reset)
  (newline)
  (write-separator))

(define (write-bar-line label successes total)
  (write-string (~a label #:width (section-title-width) #:align 'right))
  (write-string " ")

  (define score (/ successes total))
  (define score-color (cond
                        [(= score 1) color:green]
                        [(>= score 7/10) color:yellow]
                        [else color:red]))

  (term-bytes score-color)
  (define filled-chars (round (* score 50)))
  (write-chars #\█ filled-chars)
  (write-chars #\░ (- 50 filled-chars))
  (write-string " ")
  (write-string (~r (* score 100) #:precision 0 #:min-width 3))
  (write-string "%")

  (term-bytes color:gray)
  (write-string (~a " " (~r successes #:min-width (totals-width)) "/" total))
  (term-bytes color:reset)
  (newline))
