#!/usr/bin/bin/env bash

set -euo pipefail

IMAGE_NAME="congame:$GITHUB_SHA"
TARGET_HOST="deepploy@$DEPLOY_HOST"
echo "$DEPLOY_KEY" > /tmp/deploy-key
docker save "$IMAGE_NAME" | ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" docker load
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" docker stop congame-staging || true
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" docker run --name congame-staging "$IMAGE_NAME"
