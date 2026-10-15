#lang racket/base

(require racket/file
         racket/path
         rackunit
         "../private/git.rkt"
         "../private/replay.rkt"
         "../private/util.rkt"
         "../private/workspace.rkt")

(define (with-root proc)
  (define root (make-temporary-file "replay-test-~a" 'directory))
  (dynamic-wind void (lambda () (proc root)) (lambda () (delete-directory/files root))))

(define (write-text path text)
  (make-directory* (path-only path))
  (display-to-file text path #:exists 'truncate/replace))

(test-case "replay restores the recorded revision, binary additions, and deletions"
  (with-root
   (lambda (root)
     (define repository (build-path root "repo"))
     (define fixture (build-path repository "llm-benchmarks" "tasks" "test" "fixture"))
     (write-text (build-path fixture "study.rkt") "original\n")
     (write-text (build-path fixture "removed.txt") "gone\n")
     (initialize-workspace-git! repository)
     (define revision (git-head repository))
     (define workspace (build-path root "workspace"))
     (copy-directory/files fixture workspace)
     (write-text (build-path workspace "TASK.md") "original prompt\n")
     (initialize-workspace-git! workspace)
     (define result (build-path root "result"))
     (make-directory result)
     (write-text (build-path result "TASK.md") "original prompt\n")
     (write-json-file (build-path result "manifest.json")
                      (hasheq 'task_id "test" 'repository_commit revision 'repository_dirty #f
                              'task (hasheq 'fixture "fixture")))
     (write-text (build-path workspace "study.rkt") "submitted\n")
     (write-text (build-path workspace "TASK.md") "changed prompt\n")
     (delete-file (build-path workspace "removed.txt"))
     (call-with-output-file (build-path workspace "binary.dat")
       (lambda (out) (write-bytes #"\0\377\0\376" out)))
     (snapshot-workspace! workspace result '("study.rkt" "TASK.md" "removed.txt" "binary.dat"))
     (write-text (build-path fixture "study.rkt") "current checkout differs\n")
     (define destination (build-path root "restored"))
     (extract-run! repository result destination)
     (check-equal? (file->string (build-path destination "study.rkt")) "submitted\n")
     (check-equal? (file->string (build-path destination "TASK.md")) "changed prompt\n")
     (check-equal? (file->bytes (build-path destination "binary.dat")) #"\0\377\0\376")
     (check-false (file-exists? (build-path destination "removed.txt")))
     (check-exn #rx"already exists" (lambda () (extract-run! repository result destination)))
     (check-equal? (file->string (build-path fixture "study.rkt")) "current checkout differs\n"))))

(test-case "saved baselines work without repository history, including dirty runs"
  (with-root
   (lambda (root)
     (define workspace (build-path root "workspace"))
     (write-text (build-path workspace "study.rkt") "uncommitted baseline\n")
     (initialize-workspace-git! workspace)
     (define result (build-path root "result"))
     (make-directory result)
     (git-capture workspace (list "bundle" "create" (path-string (build-path result "baseline.bundle")) "HEAD"))
     (write-json-file (build-path result "manifest.json") (hasheq 'repository_dirty #t))
     (write-text (build-path workspace "study.rkt") "submitted\n")
     (snapshot-workspace! workspace result '("study.rkt"))
     (define destination (build-path root "restored"))
     (extract-run! root result destination)
     (check-equal? (file->string (build-path destination "study.rkt")) "submitted\n")
     (check-false (directory-exists? (build-path destination ".git")))
     (delete-file (build-path result "baseline.bundle"))
     (check-exn #rx"dirty checkout" (lambda () (extract-run! root result (build-path root "dirty")))))))

(test-case "failed replay leaves no destination"
  (with-root
   (lambda (root)
     (define result (build-path root "result"))
     (make-directory result)
     (define destination (build-path root "restored"))
     (check-exn #rx"no saved submission" (lambda () (extract-run! root result destination)))
     (check-false (directory-exists? destination)))))
