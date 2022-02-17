#lang racket/base

(require congame/components/study
         (prefix-in upload: congame-web/components/upload)
         koyo/haml
         racket/port
         threading
         web-server/http
         (except-in forms form))

(provide
 valid-pdf?
 file-download/link)

(define (valid-pdf? b)
  (if  (and (binding:file? b)
            (and~>
             (headers-assq* #"content-type" (binding:file-headers b))
             (header-value)
             (regexp-match? #rx#"application/pdf" _)))
       (ok b)
       (err "the file must be a PDF")))

(define (file-download/link upload-filename text
                            #:filename [filename #"file"]
                            #:content-type [content-type #"application/octet-stream"])
  (define uploader (upload:current-uploader))
  (haml
   (:a
    ([:href ((current-embed/url)
             (lambda (_req)
               (response/output
                #:headers (list
                           (make-header #"content-type" (string->bytes/utf-8 content-type))
                           (make-header #"content-disposition" (string->bytes/utf-8 (format "attachment; filename=\"~a\"" filename))))
                (lambda (out)
                  (parameterize ([upload:current-uploader uploader])
                    (upload:call-with-uploaded-file
                     upload-filename
                     (lambda (in)
                       (copy-port in out))))))))])
    text)))
