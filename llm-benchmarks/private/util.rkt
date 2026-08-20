#lang racket/base

(require file/sha1
         json
         racket/file
         racket/format
         racket/path
         racket/random)

(provide
 copy-tree!
 ensure-directory!
 file-sha1
 jref
 make-run-id
 path-string
 read-json-file
 timestamp
 write-json-file)

(define (ensure-directory! path)
  (make-directory* path)
  path)

(define (read-json-file path)
  (call-with-input-file path read-json))

(define (write-json-file path value)
  (ensure-directory! (or (path-only path) (current-directory)))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (write-json value out #:indent 2)
      (newline out))))

(define (jref value key [failure (lambda () (error 'jref "missing JSON key: ~a" key))])
  (hash-ref value key failure))

(define (path-string path)
  (path->string (simplify-path (path->complete-path path))))

(define (copy-tree! source destination)
  (cond
    [(directory-exists? source)
     (when (directory-exists? destination)
       (delete-directory/files destination))
     (ensure-directory! (or (path-only destination) (current-directory)))
     (copy-directory/files source destination)]
    [(file-exists? source)
     (ensure-directory! (or (path-only destination) (current-directory)))
     (copy-file source destination #t)]
    [else
     (error 'copy-tree! "source does not exist: ~a" source)]))

(define (file-sha1 path)
  (call-with-input-file path sha1))

(define (timestamp [seconds (current-seconds)])
  (define d (seconds->date seconds #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (~r (date-year d) #:min-width 4 #:pad-string "0")
          (~r (date-month d) #:min-width 2 #:pad-string "0")
          (~r (date-day d) #:min-width 2 #:pad-string "0")
          (~r (date-hour d) #:min-width 2 #:pad-string "0")
          (~r (date-minute d) #:min-width 2 #:pad-string "0")
          (~r (date-second d) #:min-width 2 #:pad-string "0")))

(define (make-run-id task-id harness-id repetition)
  (define stamp
    (regexp-replace* #px"[-:]" (timestamp) ""))
  (define nonce
    (substring (bytes->hex-string (crypto-random-bytes 4)) 0 8))
  (format "~a__~a__~a__r~a__~a"
          stamp task-id harness-id repetition nonce))
