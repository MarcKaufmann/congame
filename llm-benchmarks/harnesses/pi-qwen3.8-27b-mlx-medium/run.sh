#!/usr/bin/env bash
set -euo pipefail

exec pi \
  --mode json \
  --provider lmstudio \
  --model qwen3.8-27b-mlx \
  --thinking medium \
  --approve \
  @TASK.md
