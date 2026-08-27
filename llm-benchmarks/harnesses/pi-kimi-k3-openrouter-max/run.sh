#!/usr/bin/env bash
set -euo pipefail

exec pi \
  --mode json \
  --provider openrouter \
  --model moonshotai/kimi-k3 \
  --thinking max \
  --approve \
  @TASK.md
