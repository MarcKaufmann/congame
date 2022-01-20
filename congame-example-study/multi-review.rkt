#lang racket/base

(require congame/components/formular
         congame/components/study
         koyo/haml
         racket/list
         racket/random
         web-server/http)

;; TODO: support for the admin to set/trigger instance variables

;; design:
;; * the entire class is part of the same group
;; * the size of the group is predeterimened (eg. 10 people total)
;; * first, everybody has to submit a design before anybody can make progress
;; * everybody is assigned to review 2 other random people from the group

(provide
 review-study)

(define class-size 4)

;; FIXME: the worst-case complexity here is really bad.  There must be
;; a way to do this without drawing randomly and retrying on failure.
(define (assign-reviewers pids [n 2])
  (when (<= (length pids) (add1 n))
    (raise-arguments-error 'assign-reviewers "n must be > pids + 1" "pids" pids "n" n))
  (define (help)
    (for/fold ([assignments (hash)]
               [pids pids]
               #:result assignments)
              ([pid (in-list pids)])
      (define eligible (remove pid pids))
      #:break (null? eligible)
      (define choice (random-ref eligible))
      (values
       (hash-update assignments pid (λ (vs) (cons choice vs)) null)
       (remove choice pids))))
  (let loop ([assignments (hash)]
             [i n])
    (cond
      [(zero? i) assignments]
      [else
       (define next-i (sub1 i))
       (define assignments* (help))
       (define merged
         (for/hash ([(pid l) (in-hash assignments*)])
           (values pid (append l (hash-ref assignments pid null)))))
       (define ok?
         (for/and ([l (in-hash-values merged)])
           (= (length (remove-duplicates l)) (- n next-i))))
       (if ok?
           (loop merged next-i)
           (loop assignments i))])))

(define (matchmake)
  (with-study-transaction
    (define designs (get/instance 'designs (hash)))
    (cond
      [(= (hash-count designs) class-size)
       (define assignments (get/instance 'assignments #f))
       (cond
         [assignments #t]
         [else
          (define participant-ids (hash-keys designs))
          (put/instance 'assignments (assign-reviewers participant-ids))
          #t])]
      [else #f])))

(define (provide-design)
  (page
   (haml
    (.container
     (formular
      (haml
       (:div
        (#:design (input-file "Please provide a study design"))
        (:button.button.next-button ([:type "submit"]) "Submit")))
      (lambda (#:design design)
        (with-study-transaction
          (define design-file (put/instance-file design))
          (define designs (get/instance 'designs (hash)))
          (define updated-designs (hash-set designs (current-participant-id) design-file))
          (put/instance 'designs updated-designs))))))))

(define (lobby)
  (if (matchmake)
      (page
       (button
        (lambda ()
          (define all-assignments (get/instance 'assignments))
          (put 'assignments (hash-ref all-assignments (current-participant-id))))
        "Continue."))
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

(define (review)
  (define designs (get/instance 'designs))
  (define assignments (get 'assignments))
  (define design-file (get/instance-file (hash-ref designs (car assignments))))
  (page
   (haml
    (:div
     (:p "Please review this design.")
     (:p
      (:a
       ([:href ((current-embed/url)
                (lambda (_req)
                  (response/output
                   #:headers (list
                              (or (headers-assq* #"content-type" (binding:file-headers design-file))
                                  (make-header #"content-type" "application/octet-stream"))
                              (make-header #"content-disposition" (string->bytes/utf-8 (format "attachment; filename=\"~a\"" (binding:file-filename design-file)))))
                   (lambda (out)
                     (write-bytes (binding:file-content design-file) out)))))])
       "Download"))
     (button
      (lambda ()
        (put 'assignments (cdr assignments)))
      "Continue")))))

(define (final)
  (page
   (haml
    (:p "You're done. Thanks!"))))

(define review-study
  (make-study
   "review-study"
   #:requires '()
   #:provides '()
   (list
    (make-step 'provide-design provide-design)
    (make-step 'lobby lobby)
    (make-step 'review-1 review)
    (make-step 'review-2 review)
    (make-step 'final final))))
