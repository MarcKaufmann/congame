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
      ;; When a branch is checked out, .git/HEAD looks like a pointer
      ;; (ref: refs/foo), otherwise it is just the sha of a commit so
      ;; in the second case we read the whole file.
      (match (regexp-try-match #rx"^ref: refs/(.+)$" in)
        [(list _ ref) (string-trim (bytes->string/utf-8 ref))]
        [#f (string-trim (port->string in))]))))

(define current-git-sha
  (string-trim
   (call-with-input-file (build-path git-refs (parse-HEAD)) port->string)))
