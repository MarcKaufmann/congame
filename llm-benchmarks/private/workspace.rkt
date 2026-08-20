#lang racket/base

(require racket/file
         racket/path
         racket/string
         "git.rkt"
         "process.rkt"
         "util.rkt")

(provide
 (struct-out workspace-snapshot)
 apply-snapshot!
 prepare-workspace!
 snapshot-workspace!)

(struct workspace-snapshot (patch-path changed-paths untracked-paths violations) #:transparent)

(define (remove-compiled-directories! root)
  (for ([entry (in-list (directory-list root #:build? #t))])
    (when (directory-exists? entry)
      (if (equal? (path->string (file-name-from-path entry)) "compiled")
          (delete-directory/files entry)
          (remove-compiled-directories! entry)))))

(define (copy-reference! repository-root reference-root spec)
  (define source (build-path repository-root (jref spec 'source)))
  (define destination (build-path reference-root (jref spec 'destination)))
  (copy-tree! source destination)
  (remove-compiled-directories! destination))

(define (environment-appendix server-url study-id instance-slug)
  (string-append
   "\n\n## Benchmark environment\n\n"
   "- Conscript source: `../reference/conscript/`\n"
   "- Congame documentation: `../reference/congame-doc/`\n"
   "- Curated examples: `../reference/examples/` (when present)\n"
   (format "- Congame server: `~a`\n" server-url)
   (format "- Study ID: `~a`\n" study-id)
   (format "- Active instance slug: `~a`\n" instance-slug)
   (format "- Participant URL: `~a/_anon-login/~a`\n" server-url instance-slug)
   "\nUseful commands:\n\n"
   "```bash\n"
   "raco make study.rkt\n"
   (format "raco congame upload ~a study.rkt\n" study-id)
   "racket exercise.rkt  # when an exercise is part of the task\n"
   "```\n"))

(define (prepare-workspace! repository-root run-root task task-directory
                            #:server-url server-url
                            #:instance-slug instance-slug)
  (define workspace (build-path run-root "workspace"))
  (define reference-root (build-path run-root "reference"))
  (copy-tree! (build-path task-directory (jref task 'fixture)) workspace)
  (remove-compiled-directories! workspace)
  (ensure-directory! reference-root)
  (for ([reference (in-list (hash-ref task 'references null))])
    (copy-reference! repository-root reference-root reference))
  (define prompt-path (build-path task-directory (jref task 'prompt)))
  (define task-path (build-path workspace "TASK.md"))
  (call-with-output-file task-path
    #:exists 'truncate/replace
    (lambda (out)
      (display (file->string prompt-path) out)
      (display (environment-appendix server-url
                                     (jref task 'study_id)
                                     instance-slug)
               out)))
  (initialize-workspace-git! workspace)
  workspace)

(define (split-nul value)
  (filter (lambda (item) (not (string=? item "")))
          (string-split value "\0" #:trim? #f)))

(define (allowed-path? path allowed)
  (for/or ([prefix (in-list allowed)])
    (or (string=? path prefix)
        (string-prefix? path (string-append prefix "/")))))

(define (snapshot-workspace! workspace result-directory allowed-paths)
  (define-values (_u-result untracked-output _u-stderr)
    (git-capture workspace '("ls-files" "--others" "--exclude-standard" "-z")))
  (define untracked-paths (split-nul untracked-output))
  (define untracked-root (build-path result-directory "untracked-files"))
  (for ([relative (in-list untracked-paths)])
    (copy-tree! (build-path workspace relative)
                (build-path untracked-root relative)))
  ;; Intent-to-add makes the binary-safe diff include new files.
  (git-capture workspace '("add" "-N" "--" "."))
  (define-values (_n-result changed-output _n-stderr)
    (git-capture workspace '("diff" "--name-only" "HEAD")))
  (define changed-paths
    (filter (lambda (line) (not (string=? line "")))
            (string-split (string-trim changed-output) "\n")))
  (define-values (_d-result patch _d-stderr)
    (git-capture workspace '("diff" "--binary" "HEAD")))
  (define patch-path (build-path result-directory "changes.patch"))
  (call-with-output-file patch-path
    #:exists 'truncate/replace
    (lambda (out) (display patch out)))
  (workspace-snapshot
   patch-path
   changed-paths
   untracked-paths
   (filter (lambda (path) (not (allowed-path? path allowed-paths)))
           changed-paths)))

(define (apply-snapshot! fixture-directory destination patch-path)
  (copy-tree! fixture-directory destination)
  (remove-compiled-directories! destination)
  (unless (zero? (file-size patch-path))
    (define-values (result _stdout stderr)
      (run-command/capture
       "git"
       (list "apply" "--binary" (path-string patch-path))
       #:cwd destination
       #:check? #f))
    (unless (zero? (command-result-exit-code result))
      (error 'apply-snapshot! "failed to replay submission: ~a" stderr)))
  destination)
