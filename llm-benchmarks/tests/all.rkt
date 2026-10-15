#lang racket/base

(module+ test
  ;; These modules contain top-level RackUnit test cases. Loading each one runs
  ;; its tests while keeping this aggregator free of side-effect-only requires.
  (for ([test-module (in-list '("code-style.rkt"
                                "config.rkt"
                                "cost.rkt"
                                "git.rkt"
                                "pi-stream-view.rkt"
                                "pi-stream-watchdog.rkt"
                                "process.rkt"
                                "review-model.rkt"
                                "run-cli.rkt"
                                "task-definition.rkt"
                                "task-draft.rkt"
                                "workspace.rkt"))])
    (dynamic-require test-module #f)))
