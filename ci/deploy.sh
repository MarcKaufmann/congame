#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "usage: ci/deploy.sh PRODUCTION|STAGING"
    exit 1
fi

# BASEPATH="$(dirname "$0")"
DEPLOY_USER="deepploy"
TARGET_HOST="$DEPLOY_USER@$DEPLOY_HOST"

case "$1" in
    PRODUCTION)
        IDENTITY_SERVICE_NAME="congame-identity-production"
        IDENTITY_SERVICE_PORT_BLUE="8200"
        IDENTITY_SERVICE_PORT_GREEN="8300"
        IDENTITY_PATH="/opt/congame-identity-production"
        IDENTITY_HOST="identity.totalinsightmanagement.com"
        IDENTITY_ENV="identity-production"
        IDENTITY_DB_NAME="congame_identity_production"
        IDENTITY_DB_USERNAME="congame_identity_production"
        IDENTITY_DB_PASSWORD="congame_identity_production"
        WEB_SERVICE_NAME="congame-production"
        WEB_SERVICE_PORT_BLUE="8000"
        WEB_SERVICE_PORT_GREEN="8100"
        WEB_PATH="/opt/congame-web-production"
    ;;
    STAGING)
        IDENTITY_SERVICE_NAME="congame-identity-staging"
        IDENTITY_SERVICE_PORT_BLUE="9200"
        IDENTITY_SERVICE_PORT_GREEN="9300"
        IDENTITY_PATH="/opt/congame-identity-staging"
        IDENTITY_HOST="identity-staging.totalinsightmanagement.com"
        IDENTITY_ENV="identity-staging"
        IDENTITY_DB_NAME="congame_identity_staging"
        IDENTITY_DB_USERNAME="congame_identity_staging"
        IDENTITY_DB_PASSWORD="congame_identity_staging"
        WEB_SERVICE_NAME="congame-staging"
        WEB_SERVICE_PORT_BLUE="9000"
        WEB_SERVICE_PORT_GREEN="9100"
        WEB_PATH="/opt/congame-web-staging"
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

# TODO: add health check
log "Deploying identity..."
raco koyo deploy \
     --ssh-flags "-i /tmp/deploy-key" \
     --app-name "$IDENTITY_SERVICE_NAME" \
     --exec-name "congame-identity" \
     --user "$DEPLOY_USER" \
     -p blue "$IDENTITY_SERVICE_PORT_BLUE" \
     -p green "$IDENTITY_SERVICE_PORT_GREEN" \
     -e "CONGAME_IDENTITY_DB_HOST" "127.0.0.1" \
     -e "CONGAME_IDENTITY_DB_NAME" "$IDENTITY_DB_NAME" \
     -e "CONGAME_IDENTITY_DB_PASSWORD" "$IDENTITY_DB_PASSWORD" \
     -e "CONGAME_IDENTITY_DB_USERNAME" "$IDENTITY_DB_USERNAME" \
     -e "CONGAME_IDENTITY_DEBUG" "x" \
     -e "CONGAME_IDENTITY_DOMAIN_NAME" "$IDENTITY_HOST" \
     -e "CONGAME_IDENTITY_ENVIRONMENT" "$IDENTITY_ENV" \
     -e "CONGAME_IDENTITY_LOG_LEVEL" "debug" \
     -e "CONGAME_IDENTITY_POSTMARK_TOKEN" "$POSTMARK_TOKEN" \
     -e "CONGAME_IDENTITY_PRODUCT_NAME" "$IDENTITY_HOST" \
     -e "CONGAME_IDENTITY_PROFILE" "x" \
     -e "CONGAME_IDENTITY_SENTRY_DSN" "$SENTRY_DSN" \
     -e "CONGAME_IDENTITY_SESSION_SECRET_KEY_PATH" "$IDENTITY_PATH/session-secret-key" \
     -e "CONGAME_IDENTITY_SUPPORT_EMAIL" "admin@totalinsightmanagement.com" \
     -e "CONGAME_IDENTITY_SUPPORT_NAME" "Marc Kaufmann" \
     -e "CONGAME_IDENTITY_URL_HOST" "$IDENTITY_HOST" \
     -e "CONGAME_IDENTITY_URL_PORT" "443" \
     -e "CONGAME_IDENTITY_URL_SCHEME" "https" \
     -e "VERSION" "$GITHUB_SHA" \
     "$TARGET_HOST"

# DOCS_IMAGE_NAME="ghcr.io/marckaufmann/congame-docs:$GITHUB_SHA"
# IDENTITY_IMAGE_NAME="ghcr.io/marckaufmann/congame-identity:$GITHUB_SHA"
# SMTP_IMAGE_NAME="ghcr.io/marckaufmann/congame-smtp-proxy:$GITHUB_SHA"
# WEB_IMAGE_NAME="ghcr.io/marckaufmann/congame-web:$GITHUB_SHA"

# # NOTE: This gets deployed for both staging & production.
# SMTP_CONTAINER_NAME="congame-smtp-proxy"
# SMTP_CONTAINER_PORT_1="25"
# SMTP_CONTAINER_PORT_2="675"

# log "Adding GIT SHA and VERSION to identity environment file..."
# echo "VERSION=$GITHUB_SHA" >> "$IDENTITY_ENVIRONMENT_PATH"

# log "Adding GIT SHA and VERSION to environment file..."
# echo "CONGAME_WEB_GIT_SHA=$GITHUB_SHA" >> "$WEB_ENVIRONMENT_PATH"
# echo "VERSION=$GITHUB_SHA" >> "$WEB_ENVIRONMENT_PATH"

# log "Adding POSTMARK_TOKEN to identity environment file..."
# echo "CONGAME_IDENTITY_POSTMARK_TOKEN=$POSTMARK_TOKEN" >> "$IDENTITY_ENVIRONMENT_PATH"

# log "Adding POSTMARK_TOKEN to web environment file..."
# echo "CONGAME_WEB_POSTMARK_TOKEN=$POSTMARK_TOKEN" >> "$WEB_ENVIRONMENT_PATH"

# log "Adding SENTRY_DSN to identity environment file..."
# echo "CONGAME_IDENTITY_SENTRY_DSN=$SENTRY_DSN" >> "$IDENTITY_ENVIRONMENT_PATH"

# log "Adding SENTRY_DSN to web environment file..."
# echo "CONGAME_WEB_SENTRY_DSN=$SENTRY_DSN" >> "$WEB_ENVIRONMENT_PATH"

# log "Pulling image from GHCR..."
# ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
#   echo "$PAT" | docker login ghcr.io -u MarcKaufmann --password-stdin
#   docker pull "$DOCS_IMAGE_NAME"
#   docker pull "$IDENTITY_IMAGE_NAME"
#   docker pull "$SMTP_IMAGE_NAME"
#   docker pull "$WEB_IMAGE_NAME"
# EOF

# log "Restarting the container..."
# ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
#   mkdir -p "$IDENTITY_RUN_PATH"
#   mkdir -p "$WEB_RUN_PATH"
#   mkdir -p "$WEB_RUN_PATH/uploads"
# EOF

# scp -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$IDENTITY_ENVIRONMENT_PATH" "$TARGET_HOST:$IDENTITY_RUN_PATH/env"
# scp -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$WEB_ENVIRONMENT_PATH" "$TARGET_HOST:$WEB_RUN_PATH/env"
# ssh -o "StrictHostKeyChecking off" -i /tmp/deploy-key "$TARGET_HOST" <<EOF
#   docker network create congame || true

#   docker stop "$DOCS_CONTAINER_NAME" || true
#   docker rm "$DOCS_CONTAINER_NAME" || true
#   docker run \
#     --name "$DOCS_CONTAINER_NAME" \
#     --network congame \
#     -p "127.0.0.1:$DOCS_CONTAINER_PORT":"80" \
#     -d \
#     "$DOCS_IMAGE_NAME"

#   docker stop "$IDENTITY_CONTAINER_NAME" || true
#   docker rm "$IDENTITY_CONTAINER_NAME" || true
#   docker run \
#     --name "$IDENTITY_CONTAINER_NAME" \
#     --env-file "$IDENTITY_RUN_PATH/env" \
#     --network congame \
#     -v "$IDENTITY_RUN_PATH":"$IDENTITY_RUN_PATH" \
#     -p "127.0.0.1:$IDENTITY_CONTAINER_PORT":"$IDENTITY_CONTAINER_PORT" \
#     -d \
#     "$IDENTITY_IMAGE_NAME"

#   docker stop "$SMTP_CONTAINER_NAME" || true
#   docker rm "$SMTP_CONTAINER_NAME" || true
#   docker run \
#     --name "$SMTP_CONTAINER_NAME" \
#     --network congame \
#     -v /etc/letsencrypt:/etc/letsencrypt \
#     -p "0.0.0.0:$SMTP_CONTAINER_PORT_1":"$SMTP_CONTAINER_PORT_2" \
#     -p "0.0.0.0:$SMTP_CONTAINER_PORT_2":"$SMTP_CONTAINER_PORT_2" \
#     -d \
#     "$SMTP_IMAGE_NAME" \
#       --host 0.0.0.0 \
#       --ssl-key /etc/letsencrypt/live/identity-staging.totalinsightmanagement.com-0001/privkey.pem \
#       --ssl-cert /etc/letsencrypt/live/identity-staging.totalinsightmanagement.com-0001/fullchain.pem \
#       --domain "@identity.totalinsightmanagement.com" congame-identity 8675 \
#       --domain "@identity-staging.totalinsightmanagement.com" congame-identity-staging 8675

#   echo "Running SMTP server health check..."
#   if ! (echo -e 'EHLO\r\nQUIT\r\n' | nc 127.0.0.1 25); then
#     echo "SMTP server health check failed."
#     exit 1
#   fi

#   docker stop "$WEB_CONTAINER_NAME" || true
#   docker rm "$WEB_CONTAINER_NAME" || true
#   docker run \
#     --name "$WEB_CONTAINER_NAME" \
#     --network congame \
#     --env-file "$WEB_RUN_PATH/env" \
#     -v /var/run/docker.sock:/var/run/docker.sock \
#     -v "$WEB_RUN_PATH":"$WEB_RUN_PATH" \
#     -p "127.0.0.1:$WEB_CONTAINER_PORT":"$WEB_CONTAINER_PORT" \
#     -p "127.0.0.1:$WEB_DEBUGGING_PORT":"9011" \
#     -d \
#     "$WEB_IMAGE_NAME"

#   for port in $IDENTITY_CONTAINER_PORT $WEB_CONTAINER_PORT; do
#     attempts=0
#     while true; do
#       echo "Running health check..."
#       if curl -f "http://127.0.0.1:\$port" >/dev/null 2>&1; then
#         break
#       fi
#       attempts=\$((attempts + 1))
#       if [ "\$attempts" -gt 15 ]; then
#         echo "No successful health checks after 15 seconds."
#         exit 1
#       fi
#       sleep 1
#     done
#   done
# EOF
