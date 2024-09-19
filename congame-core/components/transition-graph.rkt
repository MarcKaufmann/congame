#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/file
         racket/match
         racket/port
         racket/system)

(provide
 transition-graph
 goto
 -->)

(define-syntax (goto stx)
  (raise-syntax-error 'goto "may only be used within a transition-graph form" stx))

(define-syntax (--> stx)
  (raise-syntax-error '--> "may only be used within a transition-graph form" stx))

(define-syntax (transition-graph stx)
  (define-syntax-class transition-expr
    #:literals (goto)
    #:datum-literals (done fail)
    (pattern (done)
             #:with e this-syntax
             #:with (transition ...) #'('<end>))
    (pattern (fail _)
             #:with e this-syntax
             #:with (transition ...) #'('<fail>))
    (pattern (goto id:id)
             #:with e #''id
             #:with (transition ...) #'('id))
    (pattern (f:transition-expr arg:transition-expr ...)
             #:with e #'(f.e arg.e ...)
             #:with (transition ...) #'(f.transition ... arg.transition ... ...))
    (pattern e
             #:with (transition ...) #'()))

  (define-syntax-class transition-lambda
    #:literals (λ lambda)
    (pattern ({~or λ lambda} () body-e:transition-expr ...+)
             #:with e #'(λ () body-e.e ...)
             #:with name (datum->syntax this-syntax (gensym 'anon))
             #:with (transition ...) #'(body-e.transition ... ...))
    (pattern ({~or λ lambda} name:id () body-e:transition-expr ...+)
             #:with e #'(λ () body-e.e ...)
             #:with (transition ...) #'(body-e.transition ... ...)))

  (define-splicing-syntax-class arrow
    #:literals (--> unquote)
    (pattern id:id
             #:with e #''id
             #:with (child ...) #'()

             #:with transition-e #''id
             #:with (transition ...) #'())
    (pattern (unquote tl:transition-lambda)
             #:with e #'tl.e
             #:with (child ...) #'()

             #:with transition-e #'(list '(<goto> tl.name) tl.transition ...)
             #:with (transition ...) #'())
    (pattern (unquote e:expr)
             #:with (child ...) #'()

             #:with transition-e #'e
             #:with (transition ...) #'())
    (pattern (~seq id:id --> a2:arrow)
             #:with e #''id
             #:with (child ...) #'([cons 'id a2.e] a2.child ...)

             #:with transition-e #''id
             #:with (transition ...) #'([cons 'id a2.transition-e] a2.transition ...)))

  (syntax-parse stx
    #:literals (-->)
    [(_ [arrows:arrow] ...+)
     ;; Check for entries without any edges (eg. [a]).
     (for ([edge-stx (in-list (syntax-e #'((arrows.child ...) ...)))]
           [arrow-stx (in-list (syntax-e #'(arrows ...)))])
       (when (null? (syntax-e edge-stx))
         (raise-syntax-error 'transition-graph "nodes in a graph must point to other nodes" stx arrow-stx)))

     ;; Check for steps with multiple targets.
     (for ([edges-stx (in-list (syntax-e #'((arrows.child ...) ...)))])
       (for/fold ([nodes (hasheq)])
                 ([edge-stx (in-list (syntax-e edges-stx))])
         (syntax-parse edge-stx
           #:literals (quote)
           #:datum-literals (cons)
           [(cons (quote a:id) _)
            (define sym (syntax-e #'a))
            (when (hash-has-key? nodes sym)
              (raise-syntax-error 'transition-graph "a step cannot transition to more than one follow-up step" stx edge-stx))
            (hash-set nodes sym #t)])))

     #'(hasheq
        'comptime (list arrows.transition ... ...)
        'runtime (list arrows.child ... ...))]))


;; graphs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 comptime-transitions->dot
 comptime-transitions->pdf)

(define (comptime-transitions->dot ts [out (current-output-port)])
  (define (edge a b)
    (fprintf out "  \"~a\" -> \"~a\";~n" a b))

  (fprintf out "digraph {~n")
  (for ([t (in-list ts)])
    (match t
      [`(,a (<goto> ,p-name) ,targets ...)
       (fprintf out "  \"~a\"[shape=\"diamond\"];" p-name)
       (edge a p-name)
       (for ([target (in-list targets)])
         (edge p-name target))]
      [`(,a . ,b)
       (edge a b)]))
  (fprintf out "}~n"))

(define (comptime-transitions->pdf ts)
  (define dot (find-executable-path "dot"))
  (unless dot
    (error 'comptime-transitions->pdf "dot executable not found"))
  (define filename (make-temporary-file "comptime-transitions-~a.pdf"))
  (define-values (p-in p-out) (make-pipe))
  (thread (λ ()
            (comptime-transitions->dot ts p-out)
            (close-output-port p-out)))
  (match-define (list _stdin _stdout _pid stderr control)
    (process*/ports #f p-in #f dot "-T" "pdf" "-o" filename))
  (control 'wait)
  (cond
    [(zero? (control 'exit-code)) filename]
    [else
     (define message (format "failed to generate PDF~n  error: ~a" (port->string stderr)))
     (error 'comptime-transitions->dot message)]))

(module+ test
  (require rackunit
           syntax/macro-testing)

  (check-exn
   #rx"cannot transition to more than one follow-up step"
   (lambda ()
     (convert-compile-time-error
      (let ()
        (transition-graph [a --> b --> a --> c]))))))
