#lang info

(define collection ;; review: ignore
  "llm-benchmarks")
(define deps ;; review: ignore
  '("base"
    "db-lib"
    "http-easy-lib"
    "net-lib"))
(define build-deps ;; review: ignore
  '("rackunit-lib"))
(define raco-commands ;; review: ignore
  '(("congame-llm-bench"
     (submod llm-benchmarks/main main)
     "run and grade Conscript LLM benchmarks"
     #f)))
