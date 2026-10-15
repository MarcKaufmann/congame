# Submission review

## Review order

1. Read [TASK.md](../TASK.md) and make a checklist of requested behavior and constraints.
2. Inspect the submitted files below against that checklist. Use the patch for deletions and exact changes.
3. Read [outcome.json](../outcome.json) and [checks/summary.json](../checks/summary.json). Check upload success, individual checks, and scope violations. Inspect check commands and logs to see what was actually tested; a completed run is not a correctness verdict.
4. Inspect [stdout.log](../stdout.log) for executed verification and the agent's final handoff. For Pi, render it with `llm-benchmarks/scripts/view-pi-stream.rkt <run-directory>/stdout.log` from the checkout root. A test script alone does not prove it ran.
5. Use [stderr.log](../stderr.log) and `server/` logs to investigate failures. Artifacts may be missing if a run failed early.

The temporary server is removed after the run; saved participant URLs are no longer live. If evidence is insufficient, defer grading and verify in a fresh local study instance.

## Submitted files

Final versions of added and modified files are available under `files/`. The [binary-safe patch](../changes.patch) remains the replayable source of truth.

- **Added:** [`exercise.rkt`](<files/exercise.rkt>)
- **Modified:** [`study.rkt`](<files/study.rkt>)
