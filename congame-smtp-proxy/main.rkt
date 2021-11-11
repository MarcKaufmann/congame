#lang racket/base

(require net/smtp
         net/smtp-server
         openssl
         racket/list
         racket/port
         racket/string)

(define ((handler mapping) e)
  (unless (= (length (envelope-recipients e)) 1)
    (error 'handler "only one recipient allowed"))
  (define recipient (car (envelope-recipients e)))
  (define matched?
    (for/first ([(domain port) (in-hash mapping)] #:when (regexp-match? domain recipient))
      (define-values (header lines)
        (split-data (envelope-data e)))
      (begin0 #t
        (smtp-send-message
         "127.0.0.1" #:port-no port
         (envelope-sender e)
         (envelope-recipients e)
         header
         lines))))
  (unless matched?
    (error 'handler "domain not found")))

(define (split-data data)
  (define-values (header lines)
    (split-by (regexp-split #rx#"\r\n" data) #"" bytes=?))
  (define dot-idx (index-of lines #"." bytes=?))
  (values
   (call-with-output-bytes
    (lambda (out)
      (for ([line (in-list header)])
        (write-bytes line out)
        (write-bytes #"\r\n" out))))
   (if dot-idx
       (take lines dot-idx)
       lines)))

(define (split-by lst v [equal? equal?])
  (define idx (index-of lst v equal?))
  (if idx
      (split-at lst (add1 idx))
      (values lst null)))

(module+ main
  (require koyo/logging
           racket/cmdline)
  (define-values (host port ssl-context mapping)
    (let ([key #f]
          [cert #f]
          [host "127.0.0.1"]
          [port 675]
          [mapping (make-hash)])
      (command-line
       #:program "smtp-server"
       #:once-each
       [("--ssl-key") KEY "the SSL key to use for encryption" (set! key KEY)]
       [("--ssl-cert") CERT "the SSL cert to use for encryption" (set! cert CERT)]
       [("--host") HOST "the host to bind to (default: 127.0.0.1)"
                   (set! host HOST)]
       [("--port") PORT "the port to listen on (default: 675)"
                   (define port-num (string->number PORT))
                   (unless port-num
                     (eprintf "error: PORT must be a number~n")
                     (exit 1))
                   (set! port port-num)]
       #:multi
       [("--domain") DOMAIN PORT "a mapping from a TLD to the port of an SMTP server"
                     (unless (string-prefix? DOMAIN "@")
                       (eprintf "error: DOMAIN must start with '@'~n")
                       (exit 1))
                     (define port-num (string->number PORT))
                     (unless port-num
                       (eprintf "error: PORT must be a number~n")
                       (exit 1))
                     (hash-set! mapping (byte-regexp (regexp-quote (string->bytes/utf-8 DOMAIN))) port-num)]
       #:args []
       (when (hash-empty? mapping)
         (eprintf "error: at least one --domain is required~n")
         (exit 1))
       (define ssl-context
         (and key cert (ssl-make-server-context
                        #:private-key `(pem ,key)
                        #:certificate-chain cert)))
       (values host port ssl-context mapping))))

  (define stop-logger
    (start-logger
     #:levels `((smtp-server . debug))))

  (define stop
    (start-smtp-server
     #:host host
     #:port port
     #:tls-encode (and ssl-context
                       (λ (in out
                              #:mode mode
                              #:encrypt protocol
                              #:close-original? close?)
                         (ports->ssl-ports
                          in out
                          #:mode mode
                          #:context ssl-context
                          #:encrypt protocol
                          #:close-original? close?)))
     (handler mapping)))

  (with-handlers ([exn:break? (λ (_)
                                (stop)
                                (stop-logger))])
    (sync never-evt)))
