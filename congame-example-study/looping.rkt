#lang racket/base

(require congame/components/study
         racket/format)

(provide
 looping-study
 nested-looping-study)

(define (start)
  (define round-number
    (cond
      [(get 'round-number #f) => values]
      [else (begin0 1
              (put 'round-number 1))]))
  (put-current-round-name (~a "round " round-number))
  (button
   void
   (~a "Start " (get-current-round-name))))

(define (loop)
  (put
   #:round (get-current-round-stack)
   'current-time (current-seconds))
  `(div
    "Loopy!"
    ,(button
      (lambda ()
        (put 'round-number (add1 (get 'round-number))))
      "Continue")))

(define (end)
  "Yer done.")

(define (make-looping-study [other-steps null] [end-steps (list (make-step 'end end))])
  (make-study
   "looping-study"
   #:requires '()
   #:provides '()
   (append
    (list
     (make-step 'start start))
    other-steps
    (list
     (make-step 'loop loop (lambda ()
                             (cond
                               [(string=? (get-current-round-name) "round 3")
                                (put 'round-number 1) ;; reset round number at the end
                                next]
                               [else
                                'start]))))
    end-steps)))

(define looping-study
  (make-looping-study))

(define nested-looping-study
  (make-looping-study
   (list
    (make-step/study 'nested (make-looping-study null null)))))
