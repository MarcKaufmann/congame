#lang conscript

(require buid
         racket/match)

(provide
 make-matchmaker
 ready-groups
 current-group)

(defvar*/instance pending-group conscript/matchmaking/pending-group)
(defvar*/instance ready-groups conscript/matchmaking/ready-groups)
(defvar* current-group conscript/matchmaking/current-group)

(define ((make-matchmaker group-size) page-proc)
  (if (member current-group (if-undefined ready-groups null))
      (skip)
      (with-study-transaction
        (cond
          [(not (undefined? current-group))
           (void)]
          [(if-undefined pending-group #f)
           (match-define (cons pending-group-id remaining)
             pending-group)
           (eprintf "pending: ~s~n" pending-group)
           (set! current-group pending-group-id)
           (cond
             [(= remaining 1)
              (set! ready-groups (cons pending-group-id (if-undefined ready-groups null)))
              (set! pending-group #f)]
             [else
              (set! pending-group (cons pending-group-id (sub1 remaining)))])]
          [else
           (set! current-group (buid))
           (set! pending-group (cons current-group (sub1 group-size)))])
        (page-proc))))
