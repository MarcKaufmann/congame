#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "usage: ci/deploy.sh PRODUCTION|STAGING"
    exit 1
fi

BASEPATH="$(dirname "$0")"
IMAGE_NAME="congame:$GITHUB_SHA"
TARGET_HOST="deepploy@$DEPLOY_HOST"

case "$1" in
    PRODUCTION)
        CONTAINER_NAME="congame-production"
        CONTAINER_PORT="8000"
        ENVIRONMENT_PATH="$BASEPATH/production.env"
        RUN_PATH="/var/run/congame/production"
    ;;
    STAGING)
        CONTAINER_NAME="congame-staging"
        CONTAINER_PORT="9000"
        ENVIRONMENT_PATH="$BASEPATH/staging.env"
        RUN_PATH="/var/run/congame/staging"
    ;;
    *)
        echo "error: expected $1 to be either PRODUCTION or STAGING"
        exit 1
    ;;
esac

log() {
    printf "[%s] %s\n" "$(date)" "$@"
}

log "Loading the key..."
echo "$DEPLOY_KEY" > /tmp/deploy-key
chmod 0600 /tmp/deploy-key

log "Adding GIT SHA to environment file..."
echo "CONGAME_GIT_SHA=$GITHUB_SHA" >> "$ENVIRONMENT_PATH"

if [ "$1" = "STAGING" ]; then
    # Assumes that staging is always deployed before production, which
    # is currently true due to the way ci.yml is set up and is
    # unlikely to change.
    log "Copying the image..."
    docker save "$IMAGE_NAME" | \
        ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" -C docker load
fi

log "Restarting the container..."
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
  mkdir -p "$RUN_PATH"
EOF
scp -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$ENVIRONMENT_PATH" "$TARGET_HOST:$RUN_PATH/env"
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
  docker stop "$CONTAINER_NAME" || true
  docker rm "$CONTAINER_NAME" || true
  docker run \
    --name "$CONTAINER_NAME" \
    --env-file "$RUN_PATH/env" \
    --network "host" \
    -v "$RUN_PATH":"$RUN_PATH" \
    -p "$CONTAINER_PORT":"$CONTAINER_PORT" \
    -d \
    "$IMAGE_NAME"
EOF
