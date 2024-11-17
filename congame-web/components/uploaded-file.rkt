#lang racket/base

(require congame/components/study
         (only-in forms err ok)
         hash-view
         racket/contract/base
         racket/match
         racket/port
         threading
         web-server/http
         xml
         (prefix-in upload: "upload.rkt"))

(provide
 valid-pdf?
 (hash-view-out uploaded-file)
 (contract-out
  [upload-file!
   (->* [binding:file?]
        [#:prefix (or/c #f string?)]
        uploaded-file?)]
  [uploaded-file-attachment
   (-> uploaded-file? string? xexpr?)]))

(hash-view
 uploaded-file
 (key filename content-type))

(define (upload-file! b #:prefix [pre #f])
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
   (bytes->string/utf-8 fname)
   (bytes->string/utf-8
    (or (and~>
         (binding:file-headers b)
         (headers-assq* #"content-type" _)
         (header-value))
        #"application/octet-stream"))))

(define (uploaded-file-attachment u label)
  (match-define (uploaded-file key filename content-type) u)
  (define uploader
    (upload:current-uploader))
  (attachment
   label
   #:filename filename
   ; FIXME: We should not (?) have to convert to a string, the content type should be a string by default.
   #:content-type (if (bytes? content-type)
                      (bytes->string/utf-8 content-type)
                      content-type)
   (lambda (out)
     (parameterize ([upload:current-uploader uploader])
       (upload:call-with-uploaded-file
        key
        (lambda (in)
          (copy-port in out)))))))

(define (valid-pdf? b)
  (if  (and (binding:file? b)
            (and~>
             (headers-assq* #"content-type" (binding:file-headers b))
             (header-value)
             (regexp-match? #rx#"application/pdf" _)))
       (ok b)
       (err "the file must be a PDF")))
