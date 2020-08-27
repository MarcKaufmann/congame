#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/port
         racket/runtime-path
         racket/string)

(provide
 current-git-sha)

(define-runtime-path git-head
  (build-path 'up 'up ".git" "HEAD"))

(define-runtime-path git-refs
  (build-path 'up 'up ".git" "refs"))

(define (parse-HEAD)
  (call-with-input-file git-head
    (lambda (in)
      (match (regexp-match #rx"^ref: refs/(.+)$" in)
        [(list _ ref) (string-trim (bytes->string/utf-8 ref))]))))

(define current-git-sha
  (string-trim
   (call-with-input-file (build-path git-refs (parse-HEAD)) port->string)))
