#lang racket/base

(require koyo/random
         racket/contract
         racket/format
         racket/port
         web-server/http)

(provide
 uploader?
 make-uploader
 current-uploader
 save-file!
 call-with-uploaded-file
 wrap-uploads)

(struct uploader (path))

(define current-uploader
  (make-parameter #f))

(define/contract (make-uploader root-path)
  (-> absolute-path? uploader?)
  (uploader root-path))

(define (make-file-path filename [u (current-uploader)])
  (build-path (uploader-path u) filename))

(define/contract (save-file! b)
  (-> binding:file/port? string?)
  (define filename (~a (current-inexact-milliseconds) "-" (generate-random-string)))
  (define filepath (make-file-path filename))
  (begin0 filename
    (call-with-output-file filepath
      (lambda (out)
        (write-bytes (binding:file-content b) out)))))

(define/contract (call-with-uploaded-file filename proc)
  (-> path-string? (-> input-port? any) any)
  (call-with-input-file (make-file-path filename) proc))

(define/contract (((wrap-uploads u) hdl) req)
  (-> uploader? (-> (-> request? response?)
                    (-> request? response?)))
  (parameterize ([current-uploader u])
    (hdl req)))
