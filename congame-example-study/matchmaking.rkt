#lang racket/base

(require congame/components/study
         koyo/haml
         racket/format)

(provide
 matchmaking-study)

(define (matchmake)
  (with-instance-transaction
    (define participant-id (current-participant-id))
    (define group-id (get/instance 'group-seq 1))
    (define unmatched (remq participant-id (get/instance 'unmatched null)))
    (cond
      [(null? unmatched)
       (put/instance 'unmatched (list participant-id))]

      [else
       (define group-name (~a "group-" group-id))
       (set-group-name! (car unmatched) group-name)
       (set-current-group-name! group-name)
       (put/instance 'group-seq (add1 group-id))
       (put/instance 'unmatched (cdr unmatched))])))

(define (lobby)
  (cond
    [(string=? (current-group-name) "")
     (matchmake)
     (page
      (haml
       (:div
        (:p "Please wait for more participants to join.")
        (:script
         #<<SCRIPT
setTimeout(function() {
  document.location.reload();
}, 1000);
SCRIPT
         ))))]

    [else
     (page
      (button
       void
       "Start the study."))]))

(define (show-group)
  (page
   (haml
    (:div
     (:p
      (~a "You are in group " (current-group-name)))
     (button
      (lambda ()
        (put 'done #t))
      "Continue")))))

(define (end)
  (page
   "Done."))

(define matchmaking-study
  (make-study
   "matchmaking-study"
   #:requires '()
   #:provides '()
   (list
    (make-step 'lobby lobby)
    (make-step 'show-group show-group)
    (make-step 'end end))))
