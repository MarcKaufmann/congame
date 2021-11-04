#lang racket/base

(require koyo/database
         koyo/haml
         koyo/url
         (prefix-in m: net/mime)
         racket/contract
         racket/port
         web-server/dispatchers/dispatch
         web-server/http
         "../components/auth.rkt"
         "../components/message.rkt"
         "../components/template.rkt"
         "../components/user.rkt")

(provide
 messages-page
 message-page)

(define/contract ((messages-page db) _req)
  (-> database? (-> request? response?))
  (page
   (haml
    (.container
     (:ul
      ,@(for/list ([m (in-list (list-user-messages db (user-id (current-user))))])
          (haml
           (:li
            ([:class (if (message-unread? m) "message--unread" "")])
            (:a
             ([:href (reverse-uri 'message-page (message-id m))])
             (message-subject m))
            " (from: " (message-sender m) ")"
            (when (message-unread? m)
              " (unread)")))))))))

(define/contract ((message-page db) _req id)
  (-> database? (-> request? id/c response?))
  (define msg (lookup-user-message db (user-id (current-user)) id))
  (unless msg (next-dispatcher))
  (mark-message-read! db id)
  (define text-part
    (extract-text-part (m:mime-analyze (message-data msg))))
  (page
   (haml
    (.container
     (:h1 (message-subject msg))
     (:h3 "From: " (message-sender msg))
     (:pre (if text-part
               (call-with-output-string (m:entity-body text-part))
               "<no text content>"))))))

(define (extract-text-part m)
  (define e (m:message-entity m))
  (if (and (eq? (m:entity-type e) 'text)
           (eq? (m:entity-subtype e) 'plain))
      e
      (for*/first ([p (m:entity-parts e)]
                   [tp (in-value (extract-text-part p))]
                   #:when tp)
        tp)))
