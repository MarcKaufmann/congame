#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "usage: ci/deploy.sh PRODUCTION|STAGING"
    exit 1
fi

BASEPATH="$(dirname "$0")"
IDENTITY_IMAGE_NAME="ghcr.io/marckaufmann/congame-web:$GITHUB_SHA"
WEB_IMAGE_NAME="ghcr.io/marckaufmann/congame-web:$GITHUB_SHA"
TARGET_HOST="deepploy@$DEPLOY_HOST"

case "$1" in
    PRODUCTION)
        IDENTITY_CONTAINER_NAME="congame-identity"
        IDENTITY_CONTAINER_PORT="8100"
        IDENTITY_ENVIRONMENT_PATH="$BASEPATH/identity-production.env"
        IDENTITY_RUN_PATH="/opt/congame/identity-production"
        WEB_CONTAINER_NAME="congame"
        WEB_CONTAINER_PORT="8000"
        WEB_ENVIRONMENT_PATH="$BASEPATH/production.env"
        WEB_RUN_PATH="/opt/congame/production"
    ;;
    STAGING)
        IDENTITY_CONTAINER_NAME="congame-identity-staging"
        IDENTITY_CONTAINER_PORT="9100"
        IDENTITY_ENVIRONMENT_PATH="$BASEPATH/identity-staging.env"
        IDENTITY_RUN_PATH="/opt/congame/identity-staging"
        WEB_CONTAINER_NAME="congame-staging"
        WEB_CONTAINER_PORT="9000"
        WEB_ENVIRONMENT_PATH="$BASEPATH/staging.env"
        WEB_RUN_PATH="/opt/congame/staging"
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

log "Adding GIT SHA and VERSION to identity environment file..."
echo "VERSION=$GITHUB_SHA" >> "$IDENTITY_ENVIRONMENT_PATH"

log "Adding GIT SHA and VERSION to environment file..."
echo "CONGAME_GIT_SHA=$GITHUB_SHA" >> "$WEB_ENVIRONMENT_PATH"
echo "VERSION=$GITHUB_SHA" >> "$WEB_ENVIRONMENT_PATH"

log "Adding POSTMARK_TOKEN to identity environment file..."
echo "CONGAME_IDENTITY_POSTMARK_TOKEN=$POSTMARK_TOKEN" >> "$IDENTITY_ENVIRONMENT_PATH"

log "Adding POSTMARK_TOKEN to web environment file..."
echo "CONGAME_POSTMARK_TOKEN=$POSTMARK_TOKEN" >> "$WEB_ENVIRONMENT_PATH"

log "Adding SENTRY_DSN to identity environment file..."
echo "CONGAME_IDENTITY_SENTRY_DSN=$SENTRY_DSN" >> "$IDENTITY_ENVIRONMENT_PATH"

log "Adding SENTRY_DSN to web environment file..."
echo "CONGAME_SENTRY_DSN=$SENTRY_DSN" >> "$WEB_ENVIRONMENT_PATH"

log "Pulling image from GHCR..."
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
  echo "$PAT" | docker login ghcr.io -u MarcKaufmann --password-stdin
  docker pull "$IDENTITY_IMAGE_NAME"
  docker pull "$WEB_IMAGE_NAME"
EOF

log "Restarting the container..."
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
  mkdir -p "$IDENTITY_RUN_PATH"
  mkdir -p "$WEB_RUN_PATH"
EOF

scp -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$IDENTITY_ENVIRONMENT_PATH" "$TARGET_HOST:$IDENTITY_RUN_PATH/env"
scp -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$WEB_ENVIRONMENT_PATH" "$TARGET_HOST:$WEB_RUN_PATH/env"
ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
  docker stop "$IDENTITY_CONTAINER_NAME" || true
  docker rm "$IDENTITY_CONTAINER_NAME" || true
  docker run \
    --name "$IDENTITY_CONTAINER_NAME" \
    --env-file "$IDENTITY_RUN_PATH/env" \
    --link "postgres-13" \
    -v "$IDENTITY_RUN_PATH":"$IDENTITY_RUN_PATH" \
    -p "127.0.0.1:$IDENTITY_CONTAINER_PORT":"$IDENTITY_CONTAINER_PORT" \
    -d \
    "$IDENTITY_IMAGE_NAME"

  docker stop "$WEB_CONTAINER_NAME" || true
  docker rm "$WEB_CONTAINER_NAME" || true
  docker run \
    --name "$WEB_CONTAINER_NAME" \
    --env-file "$WEB_RUN_PATH/env" \
    --link "postgres-13" \
    -v "$WEB_RUN_PATH":"$WEB_RUN_PATH" \
    -p "127.0.0.1:$WEB_CONTAINER_PORT":"$WEB_CONTAINER_PORT" \
    -d \
    "$WEB_IMAGE_NAME"

  for port in "$IDENTITY_CONTAINER_PORT $WEB_CONTAINER_PORT"; do
    attempts=0
    while true; do
      echo "Running health check..."
      if curl -f "http://127.0.0.1:\$port" >/dev/null 2>&1; then
        break
      fi
      attempts=\$((attempts + 1))
      if [ "\$attempts" -gt 15 ]; then
        echo "No successful health checks after 15 seconds."
        exit 1
      fi
      sleep 1
    done
  done
EOF
