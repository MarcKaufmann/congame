#lang racket/base

(require (only-in conscript/base %whitelist)
         pkg/path
         racket/list
         racket/match
         racket/string
         setup/collects
         threading)

(provide (all-defined-out))

;;================================================
;; Binding info

(struct binding (id module type pkg user-module user-id) #:transparent)
(struct test (module count missing-bindings unreachable-bindings) #:transparent)

;;================================================
;; Parameters

(define modules-to-check (make-parameter '()))
(define ignored-bindings (make-parameter '()))  ; Discard bindings from these modules
(define our-packages (make-parameter '()))
(define excluded-modules (make-parameter '()))

;;================================================
;; Modules

(define (module-package mod)
  (define mpi (if (module-path-index? mod) mod (module-path-index-join mod #f)))
  (with-handlers ([exn:fail? (λ (_e) #f)])
    (match (resolved-module-path-name (module-path-index-resolve mpi #t))
      [(cons (? path? p) _submods) (path->pkg p)]
      [(? path? p) (path->pkg p)]
      [_ #f])))

(define (our-module? mod)
  (and (member (module-package mod) (our-packages)) #t))

(define (conscript-modules)
  (cons 'conscript/base
        (for/list ([mod (in-list %whitelist)]
                   #:unless (member mod (excluded-modules))
                   #:when (our-module? mod))
          mod)))

(define (lib->symbol mp)
  (match mp
    [(list 'lib (? string? str))
     (~> (path-replace-extension str #"")
         path->string
         (string-trim "/main" #:left? #f)
         string->symbol)]
    [_ mp]))

(define (mpi->modpath mpi)
  (match (resolved-module-path-name (module-path-index-resolve mpi))
    [(list* (? path? p) submods)
     `(submod ,(lib->symbol (path->module-path p)) ,@submods)]
    [(? path? p) (lib->symbol (path->module-path p))]
    [name name]))

;;================================================
;; Get bindings

(define (phase-0-exports lst)
  (cond [(assv 0 lst) => cdr]
        [else '()]))

(define (exported-names mpi)
  (define-values (vars forms) (module->exports (module-path-index-resolve mpi)))
  (map car (append (phase-0-exports vars) (phase-0-exports forms))))

;; Export origins are relative to the exporting module (e.g. "html.rkt") and would otherwise
;; resolve against the current directory. Same as `module-path-index-rejoin` in scribble/search.
(define (module-path-index-rejoin mpi rel-to)
  (define-values (name base) (module-path-index-split mpi))
  (cond
    [(not name) rel-to]
    [(not base) mpi]
    [else (module-path-index-join name (module-path-index-rejoin base rel-to))]))

;; In an origin of the form (list mpi phase import-sym phase), import-sym is usually the name the
;; origin module exports. But when a binding is imported more than once, Racket records the local
;; (e.g. prefixed) name instead, which the origin module doesn't export. So check each candidate.
(define (pick-origin id origins rel-to)
  (define candidates
    (for*/list ([origin (in-list origins)]
                [name (in-list (match origin
                                 [(list _ _ import-sym _) (remove-duplicates (list import-sym id))]
                                 [_ (list id)]))])
      (cons (module-path-index-rejoin (if (pair? origin) (car origin) origin) rel-to) name)))
  (or (for/first ([c (in-list candidates)]
                  #:when (memq (cdr c) (exported-names (car c))))
        c)
      (car candidates)))

;; Returns a list of bindings containing the original identifier and module name, as well as
;; type 'proc or 'form
(define (get-bindings mod #:type t)
  (dynamic-require mod #f)
  (define-values (vars forms) (module->exports mod))
  (define mod-pkg (module-package mod))
  (define mod-mpi (module-path-index-join mod #f))
  (for/list ([export (in-list (phase-0-exports (if (eq? t 'proc) vars forms)))])
    (match-define (list id origins) export)
    (cond
      [(null? origins)
       (binding id mod t mod-pkg mod id)]
      [else
       (match-define (cons mpi orig-id) (pick-origin id origins mod-mpi))
       (binding orig-id (mpi->modpath mpi) t (module-package mpi) mod id)])))

(define (get-all-bindings mod)
  (append (get-bindings mod #:type 'proc)
          (get-bindings mod #:type 'form)))

;; pkg is #f for Racket's main collections (racket/base etc.)
(define (ignore-binding? b)
  (match-define (binding id mod _ pkg _ _) b)
  (or
   (not pkg)
   (for/or ([ignore-pat (in-list (ignored-bindings))])
     (match ignore-pat
       [(list ignore-mod ids-to-ignore ...)
        (and (equal? mod ignore-mod)
             (member id ids-to-ignore))]
       [ignore-mod
        (equal? mod ignore-mod)]))))

(define (binding-ours? b)
  (and (member (binding-pkg b) (our-packages)) #t))

(define (binding-in-mod? b m)
  (equal? (binding-module b) m))

;; Order matters: keeps the first, so conscript/base wins over e.g. conscript/html
(define (all-bindings/filter)
  (remove-duplicates
   (flatten
    (for/list ([mod (in-list (modules-to-check))])
      (filter-not ignore-binding? (get-all-bindings mod))))
   #:key (λ (b) (cons (binding-module b) (binding-id b)))))

;; 'congame/components/bot < '(submod congame/components/bot actions) < 'congame/components/bot-maker
;; and 'congame/x < 'congame-web/x
(define (module-sort-key mod)
  (match mod
    [(list 'submod base subs ...)
     (append (module-sort-key base) (map symbol->string subs))]
    [_ (string-split (~a mod) "/")]))

(define (segments<? a b)
  (cond
    [(null? b) #f]
    [(null? a) #t]
    [(string=? (car a) (car b)) (segments<? (cdr a) (cdr b))]
    [else (string<? (car a) (car b))]))

(define (bindings-list-mod<? lst-a lst-b)
  (segments<? (module-sort-key (binding-module (car lst-a)))
              (module-sort-key (binding-module (car lst-b)))))

(define (group-by-module bindings)
  (sort (group-by binding-module bindings) bindings-list-mod<?))

;;================================================
;; Checking Scribble docs

(require racket/format
         racket/promise
         scribble/xref
         setup/xref)

(define collections-xrefs (delay (load-collections-xref)))

(define (has-definition-tag? mod id)
  (and (xref-binding->definition-tag (force collections-xrefs) (list mod id) #f) #t))

;; binding? -> (or/c 'documented 'unreachable 'missing)
;; 'unreachable = documented where it comes from, but Scribble can't get there from the module
;; the user requires
(define (doc-status b)
  (cond
    [(has-definition-tag? (binding-user-module b) (binding-user-id b)) 'documented]
    [(has-definition-tag? (binding-module b) (binding-id b)) 'unreachable]
    [else 'missing]))

(define (documented? b)
  (eq? (doc-status b) 'documented))

(define (scribble-module modname)
  (~a "@;================================================\n\n"
      "@defmodule[" modname "]\n\n"))

(define (stub-scribble-def bdg)
  (match-define (binding id _mod type _pkg _user-mod _user-id) bdg)
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
