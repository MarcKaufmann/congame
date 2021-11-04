#lang racket/base

(require component
         db
         deta
         gregor
         koyo/database
         net/head
         net/smtp-server
         racket/contract
         racket/string
         threading)

(provide
 (schema-out message)
 list-user-messages
 lookup-user-message)

(define-schema message
  #:table "messages"
  ([id id/f #:primary-key #:auto-increment]
   [user-id id/f]
   [sender string/f]
   [subject string/f]
   [data binary/f]
   [(received-at (now/moment)) datetime-tz/f]))

(define (insert-user-message! db display-name sender subject data)
  (with-database-transaction [conn db]
    (define user-id
      (query-maybe-value conn (~> (from "users" #:as u)
                                  (select u.id)
                                  (where (= u.display-name ,display-name)))))
    (and user-id (insert-one! conn (make-message
                                    #:user-id user-id
                                    #:sender sender
                                    #:subject subject
                                    #:data data)))))

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


;; mail-server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-mail-server)

(define-logger mail-server)

;; NOTE: We're not going to validate the domain side of e-mail
;; addresses, which means collisions with multiple id servers are
;; possible, though very _very_ unlikely.
(struct mail-server (db stop)
  #:transparent
  #:methods gen:component
  [(define (component-start ms)
     (define stop
       (start-smtp-server
        #:port 8675
        (handle-envelope ms)))
     (struct-copy mail-server ms [stop stop]))

   (define (component-stop ms)
     ((mail-server-stop ms))
     (struct-copy mail-server ms [stop #f]))])

(define/contract (make-mail-server db)
  (-> database? mail-server?)
  (mail-server db #f))

(define ((handle-envelope ms) envel)
  (define-values (sender recipients subject data)
    (parse-envelope envel))
  (for ([display-name (in-list recipients)])
    (unless (insert-user-message! (mail-server-db ms) display-name sender subject data)
      (error 'handle-envelope "user ~s not found" display-name))))

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
