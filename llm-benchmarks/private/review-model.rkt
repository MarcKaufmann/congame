#lang racket/base

(require racket/path
         racket/port
         racket/string
         "run-cli.rkt")

(provide
 grade-complete?
 read-evidence
 review-evidence
 suggested-evidence)

;; Keep the evidence browser useful even for runs that failed before capture.
(define (review-evidence path)
  (append '("Run summary" "TASK.md" "changes.patch")
          (for*/list ([directory '("review/files" "checks" "server")]
                      #:when (directory-exists? (build-path path directory))
                      [file (in-directory (build-path path directory))]
                      #:when (and (file-exists? file) (not (link-exists? file))))
            (path->string (find-relative-path (simple-form-path path) (simple-form-path file))))
          '("stderr.log" "manifest.json")))

(define (read-evidence path selection)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (format "Unable to read this evidence: ~a" (exn-message e)))])
    (cond
      [(equal? selection "Run summary")
       (string-append
        (with-output-to-string (lambda () (display-run-summary path)))
        "\nThese checks cover only the commands listed. A completed run is not a correctness verdict.")]
      [else
       (define file (build-path path selection))
       (cond
         [(not (file-exists? file)) "Not recorded for this run."]
         [else
          ;; Avoid freezing the UI on an unexpectedly large artifact.
          (define limit (* 2 1024 1024))
          (define bytes (call-with-input-file file (lambda (in) (read-bytes limit in))))
          (define content
            (if (eof-object? bytes)
                ""
                (bytes->string/utf-8 bytes #\?)))
          (string-append
           content
           (if (> (file-size file) limit)
               "\n\n[Preview limited to the first 2 MiB. Open the original file to read the rest.]"
               ""))])])))

(define (suggested-evidence axis-id evidence #:study-path [study-path "study.rkt"])
  (cond
    [(member axis-id '("exercise_and_verification" "handoff")) "Run summary"]
    [(equal? axis-id "scope_discipline") "changes.patch"]
    [else
     (or (and (member (string-append "review/files/" study-path) evidence)
              (string-append "review/files/" study-path))
         (findf (lambda (name) (string-prefix? name "review/files/")) evidence)
         "TASK.md")]))

(define (grade-complete? axes scores minimum maximum)
  (for/and ([axis (in-list axes)])
    (define score (hash-ref scores (string->symbol (hash-ref axis 'id)) #f))
    (and (real? score) (<= minimum score maximum))))
