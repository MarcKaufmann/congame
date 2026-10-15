#lang racket/base

(require file/untar
         racket/file
         racket/path
         "git.rkt"
         "process.rkt"
         "server.rkt"
         "util.rkt"
         "workspace.rkt")

(provide
 call-with-replayed-run
 extract-run!)

(define (relative-file value label)
  (unless (and (string? value) (relative-path? value)
               (not (member 'up (explode-path value)))
               (not (member 'same (explode-path value))))
    (error 'replay "invalid ~a: ~e" label value))
  value)

(define (extract-run! repository-root result-path destination #:allow-dirty? [allow-dirty? #f])
  (when (or (directory-exists? destination) (file-exists? destination) (link-exists? destination))
    (error 'replay "destination already exists: ~a" destination))
  (define patch (build-path result-path "changes.patch"))
  (unless (file-exists? patch)
    (error 'replay "this run has no saved submission patch"))
  (define manifest (read-json-file (build-path result-path "manifest.json")))
  (define bundle (build-path result-path "baseline.bundle"))
  (define temporary (make-temporary-file "congame-replay-extract-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define baseline (build-path temporary "baseline"))
      (cond
        [(file-exists? bundle)
         (git-capture temporary (list "clone" "--quiet" (path-string bundle) (path-string baseline)))
         (delete-directory/files (build-path baseline ".git"))]
        [else
         (when (hash-ref manifest 'repository_dirty #f)
           (unless allow-dirty?
             (error 'replay "this older run used a dirty checkout and has no saved baseline; use --allow-dirty to try its committed fixture"))
           (eprintf "warning: replaying the committed fixture; uncommitted baseline changes were not saved.\n"))
         (define task (hash-ref manifest 'task))
         (define task-id (relative-file (hash-ref manifest 'task_id) "task ID"))
         (define fixture (relative-file (hash-ref task 'fixture) "fixture path"))
         (define commit (hash-ref manifest 'repository_commit))
         (unless (and (string? commit) (regexp-match? #px"^[a-fA-F0-9]{40,64}$" commit))
           (error 'replay "run has no valid recorded repository revision"))
         (define archive (build-path temporary "fixture.tar"))
         (git-capture repository-root
                      (list "archive" "--format=tar" "--output" (path-string archive)
                            (format "~a:llm-benchmarks/tasks/~a/~a" commit task-id fixture)))
         (make-directory baseline)
         (untar archive #:dest baseline)
         ;; TASK.md is generated before the agent starts, and may itself be
         ;; changed by the patch. Restore the recorded original first.
         (define prompt (build-path result-path "TASK.md"))
         (when (file-exists? prompt) (copy-file prompt (build-path baseline "TASK.md") #t))])
      (define restored (build-path temporary "restored"))
      (apply-snapshot! baseline restored patch)
      ;; Prepare everything before creating the requested destination. Never
      ;; leave a half-applied submission behind on failure.
      (define parent (or (path-only (path->complete-path destination)) (current-directory)))
      (make-directory* parent)
      (define staging (make-temporary-file ".replay-~a" 'directory parent))
      (dynamic-wind
        void
        (lambda ()
          (define ready (build-path staging "workspace"))
          (copy-directory/files restored ready)
          (rename-file-or-directory ready destination)
          destination)
        (lambda () (delete-directory/files staging))))
    (lambda () (delete-directory/files temporary))))

(define (call-with-replayed-run repository-root result-path proc #:allow-dirty? [allow-dirty? #f])
  (define manifest (read-json-file (build-path result-path "manifest.json")))
  (define task (hash-ref manifest 'task))
  (define run-root (make-temporary-file "congame-replay-~a" 'directory))
  ;; A fresh nonce prevents concurrent replays of one run sharing a database.
  (define replay-id (make-run-id "replay" "local" 1))
  (dynamic-wind
    void
    (lambda ()
      (define workspace
        (extract-run! repository-root result-path (build-path run-root "workspace")
                      #:allow-dirty? allow-dirty?))
      (define study-path
        (build-path workspace (relative-file (hash-ref task 'study_path "study.rkt") "study path")))
      (unless (file-exists? study-path)
        (error 'replay "submission has no study file: ~a" study-path))
      (call-with-benchmark-server
       repository-root run-root replay-id "replay"
       (lambda (context)
         (define log-path (build-path run-root "upload.log"))
         (define uploaded
           (call-with-output-file log-path
             (lambda (out)
               (upload-study! context (hash-ref task 'study_id) study-path
                              #:cwd workspace #:stdout out #:stderr 'stdout))))
         (unless (and (not (command-result-timed-out? uploaded))
                      (zero? (command-result-exit-code uploaded)))
           (error 'replay "submission upload failed:\n~a" (file->string log-path)))
         (define slug "replay")
         (create-study-instance! context (hash-ref task 'study_id) slug)
         (proc context workspace (format "~a/_anon-login/~a" (server-context-url context) slug)))))
    (lambda () (delete-directory/files run-root))))
