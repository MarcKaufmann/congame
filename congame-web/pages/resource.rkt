#lang racket/base

(require congame-web/components/upload
         congame/components/resource
         congame/components/study
         db
         file/unzip
         koyo/mime
         racket/path
         racket/port
         web-server/dispatchers/dispatch
         web-server/http)

(provide
 serve-resource-page
 serve-dsl-resource-page)

(define (serve-resource-page _req id [subresource #f])
  (define r (get-resource id))
  (unless r
    (next-dispatcher))
  (define full-path
    (if subresource
        (build-path (resource-path r) subresource)
        (resource-path r)))
  (define size-in-bytes
    (file-size full-path))
  (response/output
   #:mime-type (path->mime-type full-path)
   #:headers (list (header #"content-length" (string->bytes/utf-8 (number->string size-in-bytes))))
   (lambda (out)
     (call-with-input-file full-path
       (lambda (in)
         (copy-port in out))))))

(define ((serve-dsl-resource-page db) _req instance-id path-elements)
  (define instance (lookup-study-instance db instance-id))
  (unless instance (next-dispatcher))
  (define study (lookup-study-meta db (study-instance-study-id instance)))
  (define path (and study (study-meta-dsl-archive-path study)))
  (unless (and path (not (sql-null? path)))
    (next-dispatcher))
  (call-with-uploaded-file
   path
   (lambda (in)
     (define dir
       (read-zip-directory in))
     (define study-path
       (for/first ([e (in-list (zip-directory-entries dir))]
                   #:when (regexp-match? #rx#"/?study.rkt$" e))
         (bytes->path e)))
     (unless study-path
       (next-dispatcher))
     (define study-dir-path
       (path-only study-path))
     (define entry-path
       (if study-dir-path
           (apply build-path study-dir-path path-elements)
           (apply build-path path-elements)))
     (define data #"")
     (unzip-entry
      in dir (path->bytes entry-path)
      (lambda (_name _dir? entry-in)
        (set! data (port->bytes entry-in))))
     (response/output
      #:mime-type (path->mime-type entry-path)
      #:headers (list (header #"content-length" (string->bytes/utf-8 (number->string (bytes-length data)))))
      (lambda (out)
        (write-bytes data out))))))
