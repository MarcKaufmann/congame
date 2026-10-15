#lang racket/base

(require racket/cmdline
         racket/format
         racket/list
         racket/match
         threading
         "doc-check-lib.rkt")

;; Useful tool for checking documentation coverage.
;; racket -t main.rkt        → list bindings missing docs, and summary bar graphs
;; racket -t main.rkt -- -s  → print out Scribble stubs (defform/defproc) for bindings missing docs

;; Only bindings from these packages count toward the score. Bindings from other packages are
;; reported separately; bindings from Racket's main collections are skipped.
(our-packages '("conscript" "congame-core" "congame-web"))

(excluded-modules
 '(studies/real-effort/tasks-conscript))

;; All bindings exported from these modules will be checked to see if they have Scribble
;; documentation: #lang conscript itself, plus every module on its `require` whitelist that belongs
;; to one of the packages above. If a binding is being reprovided from another module, it is
;; reported under the module it was re-exported from.
(modules-to-check (conscript-modules))

;; Some bindings only exist to provide better syntax errors when a form is used
;; outside its proper context. Others are defined in private-ish modules and
;; reprovided by other modules. We can ignore these to prevent these from counting
;; against our coverage score.
;;
;; Each element in the list is either: a module path (= ignore all bindings in that module)
;;                                     list of module path and ids (= ignore just those ids)
(ignored-bindings
 '((conscript/base require %whitelist make-wrapper)
   (conscript/form map-validator
                   ~all-errors
                   ~error
                   ~errors)
   (congame/components/formular map-validator
                                ~all-errors
                                ~error
                                ~errors)
   (conscript/base ~url)))

;; Any bindings coming from Racket's main collections, as well as those in the “ignore” list above,
;; are filtered out.
(define all-bindings (all-bindings/filter))
(define-values (our-bindings third-party-bindings)
  (partition binding-ours? all-bindings))

(define (check-module-docs bindings)
  (for/list ([bindings (in-list (group-by-module bindings))])
    (define mod (binding-module (car bindings)))
    (define statuses (map doc-status bindings))
    (define (with-status s)
      (for/list ([b (in-list bindings)]
                 [st (in-list statuses)]
                 #:when (eq? st s))
        b))
    (test mod (length bindings) (with-status 'missing) (with-status 'unreachable))))

(define (list-missing test-results)
  (for ([section (in-list test-results)])
    (match-define (test mod total missing unreachable) section)
    (define miss-count (+ (length missing) (length unreachable)))
    (unless (zero? miss-count)
      (define miss-score (/ miss-count total))
      (define score-color (cond
                            [(<= miss-score 3/10) color:yellow]
                            [else color:red]))
      (term-bytes score-color)
      (displayln (format "~a  [missing ~a of ~a]:" mod miss-count total))
      (term-bytes color:reset)
      (for ([b (in-list missing)])
        (displayln (format "  ~a" (binding-id b))))
      (for ([b (in-list unreachable)])
        (displayln (format "  ~a  (documented here, but not reachable from ~a)"
                           (binding-id b)
                           (format-user-binding b)))))))

(define (format-user-binding b)
  (if (eq? (binding-id b) (binding-user-id b))
      (binding-user-module b)
      (format "~a as ~a" (binding-user-module b) (binding-user-id b))))

(define (write-summary title test-results total-count)
  (write-header title)
  (define successes 0)
  (for ([section (in-list test-results)])
    (match-define (test mod total missing unreachable) section)
    (define success-count (- total (length missing) (length unreachable)))
    (set! successes (+ successes success-count))
    (write-bar-line mod success-count total))
  (write-separator)
  (write-bar-line "Total" successes total-count))

;; for each module: show missing bindings and progress bar summary
(define (list-missing+summary)
  (define our-results (check-module-docs our-bindings))
  (define third-party-results (check-module-docs third-party-bindings))

  (section-title-width
   (~> (for/list ([b (in-list all-bindings)])
         (string-length (~a (binding-module b))))
       (apply max _)))
  (totals-width (string-length (~a (length all-bindings))))

  (list-missing our-results)
  (write-summary "Congame documentation coverage" our-results (length our-bindings))

  (unless (null? third-party-results)
    (newline)
    (list-missing third-party-results)
    (write-summary "Re-exported from other packages (not scored)"
                   third-party-results
                   (length third-party-bindings))))

;; Print out Scribble stub content for bindings missing from the docs.
(define (scribble-stub-missing)
  (define test-results (check-module-docs our-bindings))
  (for ([section (in-list test-results)])
    (match-define (test mod total missing _unreachable) section)
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

