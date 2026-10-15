#lang racket/base

(require racket/file
         rackunit
         "../private/review-model.rkt")

(test-case "review previews survive missing files and render check evidence"
  (define root (make-temporary-file "review-model-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (check-regexp-match #rx"Not recorded" (read-evidence root "TASK.md"))
                  (check-regexp-match #rx"Checks: unavailable" (read-evidence root "Run summary"))
                  (make-directory* (build-path root "review" "files"))
                  (display-to-file "#lang conscript\n" (build-path root "review" "files" "study.rkt"))
                  (check-not-false (member "review/files/study.rkt" (review-evidence root)))
                  (check-equal? (read-evidence root "review/files/study.rkt") "#lang conscript\n")
                  (check-equal? (suggested-evidence "behavioral_correctness" (review-evidence root))
                                "review/files/study.rkt")
                  (check-equal? (suggested-evidence "handoff" (review-evidence root)) "Run summary")
                  (check-equal? (suggested-evidence "exercise_and_verification"
                                                    (review-evidence root))
                                "Run summary")
                  (display-to-file "ordinary harness output\n" (build-path root "stdout.log"))
                  (check-false (member "Agent transcript" (review-evidence root)))
                  (check-false (member "stdout.log" (review-evidence root))))
                (lambda () (delete-directory/files root))))

(test-case "saving requires all scores, accepts zero and fractions, rejects invalid values"
  (define axes (list (hasheq 'id "one") (hasheq 'id "two")))
  (check-false (grade-complete? axes (hasheq 'one 4) 0 4))
  (check-true (grade-complete? axes (hasheq 'one 0 'two 3.5) 0 4))
  (for ([invalid (list #f -1 5 +nan.0 +inf.0 1+2i)])
    (check-false (grade-complete? axes (hasheq 'one 4 'two invalid) 0 4))))
