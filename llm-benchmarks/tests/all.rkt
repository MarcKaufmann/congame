#lang racket/base

(module+ test
  ;; These modules contain top-level RackUnit test cases. Loading each one runs
  ;; its tests while keeping this aggregator free of side-effect-only requires.
  (for ([test-module (in-list '("config.rkt"
                                "git.rkt"
                                "pi-stream-view.rkt"
                                "pi-stream-watchdog.rkt"
                                "process.rkt"
                                "workspace.rkt"))])
    (dynamic-require test-module #f)))
