#!/usr/bin/env bash
set -euo pipefail

run_pi() {
  racket /harness/pi-supervisor.rkt pi \
    --mode json \
    --provider openrouter \
    --model z-ai/glm-5.3-flash \
    --thinking max \
    --approve \
    "$@"
}

recovery_attempt=0
max_recovery_attempts=2

while true; do
  if (( recovery_attempt == 0 )); then
    pi_arguments=("@TASK.md")
  else
    pi_arguments=(
      --continue
      "The previous provider response stalled in a whitespace-only reasoning loop. Discard that incomplete response and continue the task from the current conversation and workspace. Congame CLI authentication is preconfigured; run raco congame upload directly and never run congame login or inspect _cli-login."
    )
  fi

  if run_pi "${pi_arguments[@]}"; then
    exit 0
  else
    status=$?
  fi

  if (( status != 86 || recovery_attempt >= max_recovery_attempts )); then
    exit "$status"
  fi

  recovery_attempt=$((recovery_attempt + 1))
  echo "GLM stream supervisor: resuming stalled Pi session ($recovery_attempt/$max_recovery_attempts)." >&2
done
