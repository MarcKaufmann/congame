#lang racket/base

(require racket/cmdline
         racket/file)

(command-line
 #:program "implementation-changed.rkt"
 #:args (baseline-path submission-path)
 (when (bytes=? (file->bytes baseline-path)
                (file->bytes submission-path))
   (eprintf "submission is identical to the task fixture\n")
   (exit 1)))
