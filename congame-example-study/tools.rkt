#lang racket/base

(require congame/components/study
         koyo/haml
         web-server/http
         (except-in forms form))

(provide
 valid-pdf?
 file-download/link)

(define (valid-pdf? b)
  (if  (and (binding:file? b)
            (regexp-match? #rx#"^%PDF-" (binding:file-content b)))
       (ok b)
       (err "the file must be a PDF")))

(define (file-download/link submission-file link-text)
  (haml
   (:a
    ([:href ((current-embed/url)
             (lambda (_req)
               (response/output
                #:headers (list
                           (or (headers-assq* #"content-type" (binding:file-headers submission-file))
                               (make-header #"content-type" "application/octet-stream"))
                           (make-header #"content-disposition" (string->bytes/utf-8 (format "attachment; filename=\"~a\"" (binding:file-filename submission-file)))))
                (lambda (out)
                  (write-bytes (binding:file-content submission-file) out)))))])
    link-text)))
