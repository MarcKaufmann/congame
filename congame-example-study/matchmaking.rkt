#lang racket/base

(require congame/components/study
         koyo/haml
         racket/format)

(provide
 matchmaking-study)

(define (matchmake)
  (with-study-transaction
    (cond
      [(string=? (get-current-group-name) "")
       (define participant-id (current-participant-id))
       (define group-id (get/instance 'group-seq 1))
       (define unmatched (remq participant-id (get/instance 'unmatched null)))
       (cond
         [(null? unmatched)
          (begin0 #f
            (put/instance 'unmatched (list participant-id)))]

         [else
          (define group-name (~a "group-" group-id))
          (begin0 #t
            (put-current-group-name #:participant-id (car unmatched) group-name)
            (put-current-group-name group-name)
            (put/instance 'group-seq (add1 group-id))
            (put/instance 'unmatched (cdr unmatched)))])]

      [else
       #t])))

(define (lobby)
  (if (matchmake)
      (page
       (button
        void
        "Start the study."))
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
          ))))))

(define (show-group)
  (page
   (haml
    (:div
     (:p
      (~a "You are in group " (get-current-group-name)))
     (button
      (lambda ()
        (put #:round (get-current-round-stack)
             #:group (get-current-group-stack)
             'done #t))
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
