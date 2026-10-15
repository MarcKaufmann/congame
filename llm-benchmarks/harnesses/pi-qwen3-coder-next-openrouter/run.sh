#!/usr/bin/env bash
set -euo pipefail

exec pi \
  --mode json \
  --provider openrouter \
  --model qwen/qwen3-coder-next \
  --thinking off \
  --approve \
  @TASK.md
