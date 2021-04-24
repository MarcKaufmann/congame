#lang racket/base

(require congame/components/study
         racket/format)

(provide
 looping-study)

;; (define (matchmake)
;;   (with-instance-transaction
;;     (define unmatched (get/instance 'unmatched))
;;     (cond
;;       [(null? unmatched)
;;        (put/instance 'unmatched (list (current-participant-id)))]

;;       [else
;;        (set-group-name! (car unmatched) "foo")
;;        (set-current-group-name! "foo")
;;        (put/instance 'unmatched (cdr unmatched))])))

(define (start)
  (define round-number
    (cond
      [(get #:round "" 'round-number #f) => values]
      [else (begin0 1
              (put #:round "" 'round-number 1))]))
  (set-current-round-name! (~a "round " round-number))
  (page
   (button
    void
    (~a "Start " (current-round-name)))))

(define (loop)
  (put 'current-time (current-seconds))
  (page
   `(div
     "Loopy!"
     ,(button
       (lambda ()
         (put #:round "" 'round-number (add1 (get #:round "" 'round-number))))
       "Continue"))))

(define (end)
  (page
   "Yer done."))

(define looping-study
  (make-study
   "looping-study"
   #:requires '()
   #:provides '()
   (list
    (make-step 'start start)
    (make-step 'loop loop (lambda ()
                            (if (string=? (current-round-name) "round 5")
                                'end
                                'start)))
    (make-step 'end end))))
