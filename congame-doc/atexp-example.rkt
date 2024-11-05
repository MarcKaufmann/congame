#lang racket/base

;; From https://github.com/racket/scribble/blob/master/scribble-doc/scribblings/scribble/reader.scrbl

(require scribble/core
         scribble/manual
         (prefix-in racket: scribble/racket)
         (prefix-in scribble: scribble/reader))

(provide scribble-examples)

(define (as-flow e)
  (if (block? e) e (make-paragraph plain (list e))))

(define (litchar/lines . strs)
  (let ([strs (regexp-split #rx"\n" (apply string-append strs))])
    (if (= 1 (length strs))
      (litchar (car strs))
      (make-table
       plain
       (map (lambda (s) ; the nbsp is needed for IE
              (list (as-flow (if (string=? s "") 'nbsp (litchar s)))))
            strs)))))

(define spacer (hspace 2))

(define ((norm-spacing base) p)
  (cond [(and (syntax->list p) (not (null? (syntax-e p))))
         (let loop ([e (syntax->list p)]
                    [line (syntax-line (car (syntax-e p)))]
                    [pos base]
                    [second #f]
                    [accum null])
           (if (null? e)
             (datum->syntax
              p (reverse accum)
              (list (syntax-source p) (syntax-line p) base (add1 base)
                    (- pos base))
              p)
             (let* ([v ((norm-spacing (if (= line (syntax-line (car e)))
                                        pos
                                        (or second pos)))
                        (car e))]
                    [next-pos (+ (syntax-column v) (syntax-span v) 1)])
               (loop (cdr e)
                     (syntax-line v)
                     next-pos
                     (or second next-pos)
                     (cons v accum)))))]
        [else (datum->syntax
               p (syntax-e p)
               (list (syntax-source p) (syntax-line p) base (add1 base) 1)
               p)]))

(define (scribble-examples . lines)
  (define reads-as (make-paragraph plain (list spacer "reads as" spacer)))
  (let* ([lines (apply string-append lines)]
         [p (open-input-string lines)])
    (port-count-lines! p)
    (let loop ([r '()] [newlines? #f])
      (regexp-match? #px#"^[[:space:]]*" p)
      (let* ([p1  (file-position p)]
             [stx (scribble:read-syntax #f p)]
             [p2  (file-position p)])
        (if (not (eof-object? stx))
          (let ([str (substring lines p1 p2)])
            (loop (cons (list str stx) r)
                  (or newlines? (regexp-match? #rx#"\n" str))))
          (let* ([r (reverse r)]
                 [r (if newlines?
                      (cdr (apply append (map (lambda (x) (list #f x)) r)))
                      r)])
            (make-table
             plain
             (map (lambda (x)
                    (let ([@expr (if x (litchar/lines (car x)) "")]
                          [sexpr (if x
                                   (racket:to-paragraph
                                    ((norm-spacing 0) (cadr x)))
                                   "")]
                          [reads-as (if x reads-as "")])
                      (map as-flow (list spacer @expr reads-as sexpr))))
                  r))))))))