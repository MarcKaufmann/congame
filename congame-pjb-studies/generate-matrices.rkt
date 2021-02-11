#lang racket/base

(require 2htdp/image
         racket/contract
         racket/file
         racket/list
         racket/string
         racket/random)

(provide
 list->file
 list-of-small-matrices
 list-of-large-matrices)

(define (0-or-1 [percentage-ones 50])
  (if (> (random 100) percentage-ones) 0 1))

(define lowest-percentage 30)
(define highest-percentage 60)

(define (matrix-table rows columns)
  (for/list ([i rows])
    (let ([rp (random lowest-percentage highest-percentage)])
      (for/list ([j columns])
        (0-or-1 rp)))))

(define (count-ones matrix)
  (apply + (flatten matrix)))

(define (cell->image cell)
  (define size 26)
  (define text-size 16)
  (define color "black")
  (define background-square
    (overlay/align "middle" "middle"
                   (square size 'outline 'black)
                   (square size 'solid 'white)))

  (overlay/align "middle" "middle"
                 (text (if (equal? cell 1) "1" (random-ref '("0" "0" "0" "00" "11" "10")))
                       text-size
                       color)
                 background-square))

(define (row->image row)
  (apply beside (map cell->image row)))

(define (matrix->image matrix)
  (cond [(empty? matrix) empty-image]
        [(above (row->image (first matrix))
                (matrix->image (rest matrix)))]))
  
(define (add-border image)
  (define h (image-height image))
  (define w (image-width image))
  (overlay/align "middle" "middle"
                 image
                 (rectangle w h 'solid 'white)
                 (rectangle (+ w 5) (+ h 5) 'solid 'black)))

(define/contract (list->file lst
                             [a-file (build-path (current-directory) "matrices")])
  (-> (listof string?) path-string? void?)
  (display-lines-to-file lst
                         a-file
                         #:exists 'replace
                         #:mode 'text))

(define (matrix-name i type)
  (string-append type "_matrix_" (number->string i) ".png"))

(define (save-matrix m-image name img-path)
  (save-image
   m-image
   (build-path img-path name)))

(define small-nrows 7)
(define small-ncols 12)
(define large-nrows 10)
(define large-ncols 15)

(define/contract (list-of-large-matrices n dir)
  (-> integer? path-string? (listof string?))
  (for/list ([i (range 0 n)])
    (displayln i)
    (define m (matrix-table large-nrows large-ncols))
    (define ones (count-ones m))
    (define m-image (add-border (matrix->image m)))
    (define m-name (matrix-name i "large"))
    (save-matrix m-image m-name dir)
    (string-join (list (number->string i)  (number->string ones) m-name) ",")))

(define/contract (list-of-small-matrices n dir)
  (-> integer? path-string? (listof string?))
  (for/list ([i (range 0 n)])
    (displayln i)
    (define m (matrix-table small-nrows small-ncols))
    (define ones (count-ones m))
    (define m-image (add-border (matrix->image m)))
    (define m-name (matrix-name i "small"))
    (save-matrix m-image m-name dir)
    (string-join (list (number->string i)  (number->string ones) m-name) ",")))