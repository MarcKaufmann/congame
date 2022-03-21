#lang racket/base

(require congame/components/export
         congame/components/study
         (prefix-in upload: congame-web/components/upload)
         koyo/haml
         racket/match
         racket/port
         racket/serialize
         threading
         web-server/http
         (except-in forms form))

(provide
 (struct-out uploaded-file)
 valid-pdf?
 upload-file!
 file-download/link)

(serializable-struct uploaded-file
  (key
   filename
   content-type)
  #:transparent
  #:methods gen:jsexprable
  [(define (->jsexpr u)
     (hasheq
      'key (uploaded-file-key u)
      'filename (bytes->string/utf-8 (uploaded-file-filename u))
      'content-type (bytes->string/utf-8 (uploaded-file-content-type u))))])

(define (valid-pdf? b)
  (if  (and (binding:file? b)
            (and~>
             (headers-assq* #"content-type" (binding:file-headers b))
             (header-value)
             (regexp-match? #rx#"application/pdf" _)))
       (ok b)
       (err "the file must be a PDF")))

(define (upload-file! b #:prefix (pre #f))
  (define base-filename
    (binding:file-filename b))
  (define fname
    (if pre
        (~> base-filename
            bytes->string/utf-8
            (string-append pre "-" _)
            string->bytes/utf-8)
        base-filename))
  (uploaded-file
   (upload:save-file! b)
   fname
   (or (and~>
        (binding:file-headers b)
        (headers-assq* #"content-type" _)
        (header-value))
       #"application/octet-stream")))

(define (file-download/link u text)
  (match-define (uploaded-file key filename content-type) u)
  (define uploader
    (upload:current-uploader))
  (haml
   (:a
    ([:href ((current-embed/url)
             (lambda (_req)
               (response/output
                #:headers (list
                           (make-header #"content-type" content-type)
                           (make-header #"content-disposition" (string->bytes/utf-8 (format "attachment; filename=\"~a\"" filename))))
                (lambda (out)
                  (parameterize ([upload:current-uploader uploader])
                    (upload:call-with-uploaded-file
                     key
                     (lambda (in)
                       (copy-port in out))))))))])
    text)))
