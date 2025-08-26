#lang racket/base

(require racket/cmdline
         racket/format
         racket/match
         threading
         "doc-check-lib.rkt")

;; Useful tool for checking documentation coverage.
;; racket -t main.rkt        → list bindings missing docs, and summary bar graphs
;; racket -t main.rkt -- -s  → print out Scribble stubs (defform/defproc) for bindings missing docs

;; All bindings exported from these modules will be checked to see if they have Scribble
;; documentation. If a binding is being reprovided from another module, its source will be
;; given as the module where it was originally defined.
(modules-to-check
 '(conscript/base
   conscript/form0
   conscript/game-theory
   conscript/html
   conscript/markdown
   conscript/matchmaking
   conscript/resource
   conscript/survey-tools
   conscript/tasks))

;; Some bindings only exist to provide better syntax errors when a form is used
;; outside its proper context. Others are defined in private-ish modules and
;; reprovided by other modules. We can ignore these to prevent these from counting
;; against our coverage score.
;;
;; Each element in the list is either: a module path (= ignore all bindings in that module)
;;                                     list of module path and ids (= ignore just those ids)
(ignored-bindings
 '((conscript/base require)
   (conscript/form map-validator
                   ~all-errors
                   ~error
                   ~errors)
   (congame/components/formular map-validator
                                ~all-errors
                                ~error
                                ~errors)
   (conscript/base ~url)
   forms))

;; Any bindings coming from racket/*, as well as those in the “ignore” list above,
;; are filtered out.
(define all-bindings (all-bindings/filter))

(define (check-module-docs)
  (for/list ([bindings (in-list (group-by-module all-bindings))])
    (define mod (binding-module (car bindings)))
    (define bindings-not-in-docs
      (for/list ([b (in-list bindings)]
                 #:when (not (documented? b)))
        b))
    (test mod (length bindings) bindings-not-in-docs)))

;; for each module: show missing bindings and progress bar summary
(define (list-missing+summary)
  (define test-results (check-module-docs))
  (for ([section (in-list test-results)])
    (match-define (test mod total missing) section)
    (define miss-count (length missing))
    (unless (zero? miss-count)
      (define miss-score (/ miss-count total))
      (define score-color (cond
                            [(<= miss-score 3/10) color:yellow]
                            [else color:red]))
      (term-bytes score-color)
      (displayln (format "~a  [missing ~a of ~a]:" mod (length missing) total))
      (term-bytes color:reset)
      (for ([b (in-list missing)])
        (displayln (format "  ~a" (binding-id b))))))

  (section-title-width
   (~> (for/list ([b (in-list all-bindings)])
         (string-length (~a (binding-module b))))
       (apply max _)))
  (totals-width (string-length (~a (length all-bindings))))
  (write-header "Congame documentation coverage")

  (define successes 0)
  (for ([section (in-list test-results)])
    (match-define (test mod total missing) section)
    (define success-count (- total (length missing)))
    (set! successes (+ successes success-count))
    (write-bar-line mod success-count total))

  (write-separator)
  (write-bar-line "Total" successes (length all-bindings)))

;; Print out Scribble stub content for bindings missing from the docs.
(define (scribble-stub-missing)
  (define test-results (check-module-docs))
  (for ([section (in-list test-results)])
    (match-define (test mod total missing) section)
    (unless (null? missing)
      ;(term-bytes color:red color:bold)
      (write-string (scribble-module mod))
      ;(term-bytes color:reset color:yellow)
      (for ([b (in-list missing)])
        (write-string (stub-scribble-def b)))))
  (term-bytes color:reset))
  
(module+ main
  (define proc (make-parameter list-missing+summary))
  (command-line
   #:program "doc check"
   #:once-any
   [("-s" "--stub-scribble") "Produce Scribble stubs for bindings without documentation"
                             (proc scribble-stub-missing)])
  ((proc)))

