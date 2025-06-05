#lang racket/base

(require racket/class)

(provide
 indent)

(define scribble-indent
  (dynamic-require 'scribble/private/indentation 'determine-spaces))

(define (indent textoid pos)
  ;; Precedence rules:
  ;;   * Inside square brackets, indentation is based on 1+ the position of the open
  ;;   square bracket based on `backward-containing-sexp'.
  ;;   * Inside curly brackets, indentation is based on the position of the preceeding @
  ;;   symbol, after skipping any square bracket pair.
  (define sb-pos (get-open-bracket textoid pos #\[ #\]))
  (define cb-pos (get-open-bracket textoid pos))
  #;(eprintf "sb-pos: ~s (~s)~n" sb-pos (and sb-pos (send textoid get-character sb-pos)))
  #;(eprintf "cb-pos: ~s (~s)~n" cb-pos (and cb-pos (send textoid get-character cb-pos)))
  (cond
    [sb-pos
     (+ (get-column textoid sb-pos) 1)]
    [(and cb-pos (get-@-pos textoid (sub1 cb-pos)))
     => (λ (@-pos)
          (+ (get-column textoid @-pos) 2))]
    [else
     (scribble-indent textoid pos)]))

;; Finds the sexp enclosing `pos' that contains an unbalanced open
;; curly bracket and returns the position of the bracket.
(define (get-open-bracket textoid pos [open-char #\{] [close-char #\}])
  (let* ([pos (send textoid skip-whitespace pos 'backward #t)]
         [pos (send textoid backward-containing-sexp pos 0)]
         [pos (and pos (send textoid skip-whitespace pos 'backward #t))]
         #;[_ (eprintf "pos: ~s (~s)~n" pos (and pos (send textoid get-character pos)))]
         ;; Ensure the search is constrained to cases where the surrounding s-exp uses
         ;; open-char brackets. Otherwise, the enclosing s-exp might use a different kind
         ;; of brackets, and the opening line might contain our kind of brackets, in which
         ;; case we'll report the wrong position (that of our kind of bracket).
         [pos (and pos (eqv? (send textoid get-character (sub1 pos)) open-char) pos)]
         [pos (and pos (send textoid skip-whitespace pos 'backward #t))])
    (define para (and pos (send textoid position-paragraph pos)))
    (define para-lo (and para (send textoid paragraph-start-position para)))
    (define text (and pos (get-paragraph textoid pos)))
    (define idx (and text (get-bracket-pos text open-char close-char)))
    (and idx (+ para-lo idx))))

(define (get-matching-bracket textoid pos bracket-char)
  (let* ([pos (send textoid skip-whitespace pos 'backward #t)]
         [pos (send textoid backward-containing-sexp pos 0)]
         [pos (and pos (sub1 (send textoid skip-whitespace pos 'backward #t)))])
    (and pos (eqv? (send textoid get-character pos) bracket-char) pos)))

(define (get-@-pos textoid pos)
  (let* ([pos (send textoid skip-whitespace pos 'backward #t)]
         [pos (cond
                [(eqv? (send textoid get-character pos) #\])
                 (define bracket-pos (get-matching-bracket textoid pos #\[))
                 (and bracket-pos (sub1 bracket-pos))]
                [else pos])])
    (and pos (find-character-backwards textoid pos #\@ '(#\( #\[ #\{)))))

(define (get-column textoid pos)
  (define para (send textoid position-paragraph pos))
  (define para-lo (send textoid paragraph-start-position para))
  (- pos para-lo))

(define (get-bracket-pos text [open-char #\{] [close-char #\}])
  (define poss
    (for/fold ([s null])
              ([(c idx) (in-indexed (in-string text))])
      (cond
        [(eqv? c open-char)
         (cons idx s)]
        [(eqv? c close-char)
         (if (null? s) null (cdr s))]
        [else s])))
  (and (not (null? poss))
       (car poss)))

(define (get-paragraph textoid pos)
  (define para (send textoid position-paragraph pos))
  (define para-lo (send textoid paragraph-start-position para))
  (define para-hi (send textoid paragraph-end-position para))
  (send textoid get-text para-lo para-hi))

(define (find-character-backwards textoid pos needle [stop-chars null])
  (define c (send textoid get-character pos))
  (cond
    [(eqv? c needle) pos]
    [(memv c stop-chars) #f]
    [(zero? pos) #f]
    [else (find-character-backwards textoid (sub1 pos) needle stop-chars)]))
