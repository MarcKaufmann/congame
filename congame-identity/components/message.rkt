#lang racket/base

(require component
         db
         deta
         koyo/database
         net/head
         net/smtp-server
         racket/contract
         racket/string
         threading
         "mail.rkt"
         "message-struct.rkt"
         "user.rkt")

(provide
 (schema-out message)
 list-user-messages
 lookup-user-message
 mark-message-read!)

(define (insert-user-message! db user-id sender subject data)
  (with-database-connection [conn db]
    (insert-one! conn (make-message
                       #:user-id user-id
                       #:sender sender
                       #:subject subject
                       #:data data))))

(define/contract (list-user-messages db user-id)
  (-> database? id/c (listof message?))
  (with-database-connection [conn db]
    (for/list ([m (in-entities conn (~> (from message #:as m)
                                        (where (= m.user-id ,user-id))
                                        (order-by ([m.received-at #:desc]))))])
      m)))

(define/contract (lookup-user-message db user-id msg-id)
  (-> database? id/c id/c (or/c #f message?))
  (with-database-connection [conn db]
    (lookup conn (~> (from message #:as m)
                     (where (and (= m.id ,msg-id)
                                 (= m.user-id ,user-id)))))))

(define/contract (mark-message-read! db msg-id)
  (-> database? id/c void?)
  (with-database-connection [conn db]
    (query-exec conn (~> (from message #:as m)
                         (where (= m.id ,msg-id))
                         (update [unread? #f])))))


;; mail-server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-mail-server)

(define-logger mail-server)

;; NOTE: We're not going to validate the domain side of e-mail
;; addresses, which means collisions with multiple id servers are
;; possible, though very _very_ unlikely.
(struct mail-server (db mailer users stop)
  #:transparent
  #:methods gen:component
  [(define (component-start ms)
     (define stop
       (start-smtp-server
        #:host "0.0.0.0"
        #:port 8675
        (handle-envelope ms)))
     (struct-copy mail-server ms [stop stop]))

   (define (component-stop ms)
     ((mail-server-stop ms))
     (struct-copy mail-server ms [stop #f]))])

(define/contract (make-mail-server db m users)
  (-> database? mailer? user-manager? mail-server?)
  (mail-server db m users #f))

(define ((handle-envelope ms) envel)
  (define db (mail-server-db ms))
  (define users (mail-server-users ms))
  (define-values (sender recipients subject data)
    (parse-envelope envel))
  (for ([display-name (in-list recipients)])
    (define u (user-manager-lookup/display-name users display-name))
    (unless u
      (error 'handle-envelope "user ~s not found" display-name))
    (define msg
      (insert-user-message! db (user-id u) sender subject data))
    (mailer-send-message-notification-email (mail-server-mailer ms) u msg)))

(define (parse-envelope envel)
  (define data (envelope-data envel))
  (define sender
    (bytes->string/utf-8
     (or
      (extract-field #"reply-to" data)
      (extract-field #"from" data)
      (envelope-sender envel))))
  (define subject
    (bytes->string/utf-8
     (or (extract-field #"subject" data) #"<No Subject>")))
  (define recipient
    (for/list ([bs (in-list (envelope-recipients envel))])
      (define rcpt (bytes->string/utf-8 bs))
      (define addr (car (extract-addresses rcpt 'address)))
      (car (string-split addr "@"))))
  (values sender recipient subject data))
