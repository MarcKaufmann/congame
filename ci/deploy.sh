#!/usr/bin/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "usage: ci/deploy.sh PRODUCTION|STAGING"
    exit 1
fi

IMAGE_NAME="congame:$GITHUB_SHA"
TARGET_HOST="deepploy@$DEPLOY_HOST"

case "$1" in
    PRODUCTION)
        CONTAINER_NAME="congame-production"
    ;;
    STAGING)
        CONTAINER_NAME="congame-staging"
    ;;
    *)
        echo "error: expected $1 to be either PRODUCTION or STAGING"
        exit 1
    ;;
esac


echo "$DEPLOY_KEY" > /tmp/deploy-key
docker save "$IMAGE_NAME" | ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" docker load
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" docker stop "$CONTAINER_NAME" || true
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" docker run --name "$CONTAINER_NAME" "$IMAGE_NAME"
