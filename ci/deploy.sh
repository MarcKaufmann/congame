#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
    echo "usage: ci/deploy.sh PRODUCTION|STAGING"
    exit 1
fi

DEPLOY_USER="deepploy"
TARGET_HOST="$DEPLOY_USER@$DEPLOY_HOST"

IDENTITY_SMTP_PORT_PRODUCTION=8675
IDENTITY_SMTP_PORT_STAGING=8676

case "$1" in
    PRODUCTION)
        IDENTITY_SERVICE_NAME="congame-identity-production"
        IDENTITY_SERVICE_PORT_BLUE="8200"
        IDENTITY_SERVICE_PORT_GREEN="8300"
        IDENTITY_PATH="/home/$DEPLOY_USER/$IDENTITY_SERVICE_NAME"
        IDENTITY_HOST="identity.totalinsightmanagement.com"
        IDENTITY_ENV="identity-production"
        IDENTITY_DB_NAME="congame_identity_production"
        IDENTITY_DB_USERNAME="congame_identity_production"
        IDENTITY_DB_PASSWORD="congame_identity_production"
        IDENTITY_SMTP_PORT="$IDENTITY_SMTP_PORT_PRODUCTION"
        WEB_SERVICE_NAME="congame-production"
        WEB_SERVICE_PORT_BLUE="8000"
        WEB_SERVICE_PORT_GREEN="8100"
        WEB_PATH="/home/$DEPLOY_USER/$WEB_SERVICE_NAME"
        WEB_HOST="totalinsightmanagement.com"
        WEB_ENV="web-production"
        WEB_DB_NAME="congame_web_production"
        WEB_DB_USERNAME="congame_web_production"
        WEB_DB_PASSWORD="congame_web_production"
        WEB_IDENTITY_URL="identity.totalinsightmanagement.com"
        WEB_UPLOADS_DIR="$WEB_PATH/uploads"
    ;;
    STAGING)
        IDENTITY_SERVICE_NAME="congame-identity-staging"
        IDENTITY_SERVICE_PORT_BLUE="9200"
        IDENTITY_SERVICE_PORT_GREEN="9300"
        IDENTITY_PATH="/home/$DEPLOY_USER/$IDENTITY_SERVICE_NAME"
        IDENTITY_HOST="identity-staging.totalinsightmanagement.com"
        IDENTITY_ENV="identity-staging"
        IDENTITY_DB_NAME="congame_identity_staging"
        IDENTITY_DB_USERNAME="congame_identity_staging"
        IDENTITY_DB_PASSWORD="congame_identity_staging"
        IDENTITY_SMTP_PORT="$IDENTITY_SMTP_PORT_STAGING"
        WEB_SERVICE_NAME="congame-staging"
        WEB_SERVICE_PORT_BLUE="9000"
        WEB_SERVICE_PORT_GREEN="9100"
        WEB_PATH="/home/$DEPLOY_USER/$WEB_SERVICE_NAME"
        WEB_HOST="staging.totalinsightmanagement.com"
        WEB_ENV="web-staging"
        WEB_DB_NAME="congame_web_staging"
        WEB_DB_USERNAME="congame_web_staging"
        WEB_DB_PASSWORD="congame_web_staging"
        WEB_IDENTITY_URL="identity-staging.totalinsightmanagement.com"
        WEB_UPLOADS_DIR="$WEB_PATH/uploads"
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

log "Deploying SMTP server..."
raco koyo deploy \
     --ssh-flags "-i /tmp/deploy-key" \
     --app-name "congame-smtp-proxy" \
     --destination "/home/$DEPLOY_USER/congame-smtp-proxy" \
     --exec-name "congame-smtp-proxy" \
     --exec-flags "--host 0.0.0.0 \
--ssl-key /etc/letsencrypt/live/identity-staging.totalinsightmanagement.com-0001/privkey.pem \
--ssl-cert /etc/letsencrypt/live/identity-staging.totalinsightmanagement.com-0001/fullchain.pem \
--domain '@identity.totalinsightmanagement.com' 127.0.0.1 $IDENTITY_SMTP_PORT_PRODUCTION \
--domain '@identity-staging.totalinsightmanagement.com' 127.0.0.1 $IDENTITY_SMTP_PORT_STAGING" \
     --user "$DEPLOY_USER" \
     "build/smtp-proxy" "$GITHUB_SHA" "$TARGET_HOST"

log "Deploying identity..."
raco koyo deploy \
     --ssh-flags "-i /tmp/deploy-key" \
     --app-name "$IDENTITY_SERVICE_NAME" \
     --destination "$IDENTITY_PATH" \
     --exec-name "congame-identity" \
     --user "$DEPLOY_USER" \
     --health-check \
     -p blue "$IDENTITY_SERVICE_PORT_BLUE" \
     -p green "$IDENTITY_SERVICE_PORT_GREEN" \
     -e "CONGAME_IDENTITY_DB_HOST" "127.0.0.1" \
     -e "CONGAME_IDENTITY_DB_PORT" "5434" \
     -e "CONGAME_IDENTITY_DB_NAME" "$IDENTITY_DB_NAME" \
     -e "CONGAME_IDENTITY_DB_PASSWORD" "$IDENTITY_DB_PASSWORD" \
     -e "CONGAME_IDENTITY_DB_USERNAME" "$IDENTITY_DB_USERNAME" \
     -e "CONGAME_IDENTITY_DOMAIN_NAME" "$IDENTITY_HOST" \
     -e "CONGAME_IDENTITY_ENVIRONMENT" "$IDENTITY_ENV" \
     -e "CONGAME_IDENTITY_LOG_LEVEL" "debug" \
     -e "CONGAME_IDENTITY_POSTMARK_TOKEN" "$POSTMARK_TOKEN" \
     -e "CONGAME_IDENTITY_PRODUCT_NAME" "$IDENTITY_HOST" \
     -e "CONGAME_IDENTITY_SENTRY_DSN" "$SENTRY_DSN" \
     -e "CONGAME_IDENTITY_SESSION_SECRET_KEY_PATH" "$IDENTITY_PATH/session-secret-key" \
     -e "CONGAME_IDENTITY_SMTP_PORT" "$IDENTITY_SMTP_PORT" \
     -e "CONGAME_IDENTITY_SUPPORT_EMAIL" "admin@totalinsightmanagement.com" \
     -e "CONGAME_IDENTITY_SUPPORT_NAME" "Marc Kaufmann" \
     -e "CONGAME_IDENTITY_URL_HOST" "$IDENTITY_HOST" \
     -e "CONGAME_IDENTITY_URL_PORT" "443" \
     -e "CONGAME_IDENTITY_URL_SCHEME" "https" \
     -e "VERSION" "$GITHUB_SHA" \
     "build/identity" "$GITHUB_SHA" "$TARGET_HOST"

# TODO: add health check
log "Deploying web..."
raco koyo deploy \
     --ssh-flags "-i /tmp/deploy-key" \
     --app-name "$WEB_SERVICE_NAME" \
     --destination "$WEB_PATH" \
     --exec-name "congame-web" \
     --user "$DEPLOY_USER" \
     --pre-script "web-pre-script.sh" \
     -p blue "$WEB_SERVICE_PORT_BLUE" \
     -p green "$WEB_SERVICE_PORT_GREEN" \
     -e "CONGAME_WEB_DB_HOST" "127.0.0.1" \
     -e "CONGAME_WEB_DB_PORT" "5434" \
     -e "CONGAME_WEB_DB_NAME" "$WEB_DB_NAME" \
     -e "CONGAME_WEB_DB_PASSWORD" "$WEB_DB_PASSWORD" \
     -e "CONGAME_WEB_DB_USERNAME" "$WEB_DB_USERNAME" \
     -e "CONGAME_WEB_DOMAIN_NAME" "$WEB_HOST" \
     -e "CONGAME_WEB_ENVIRONMENT" "$WEB_ENV" \
     -e "CONGAME_WEB_IDENTITY_URL" "$WEB_IDENTITY_URL" \
     -e "CONGAME_WEB_LOG_LEVEL" "debug" \
     -e "CONGAME_WEB_POSTMARK_TOKEN" "$POSTMARK_TOKEN" \
     -e "CONGAME_WEB_PRODUCT_NAME" "$WEB_HOST" \
     -e "CONGAME_WEB_SENTRY_DSN" "$SENTRY_DSN" \
     -e "CONGAME_WEB_SESSION_SECRET_KEY_PATH" "$WEB_PATH/session-secret-key" \
     -e "CONGAME_WEB_SUPPORT_EMAIL" "admin@totalinsightmanagement.com" \
     -e "CONGAME_WEB_SUPPORT_NAME" "Marc Kaufmann" \
     -e "CONGAME_WEB_UPLOADS_DIR" "$WEB_UPLOADS_DIR" \
     -e "CONGAME_WEB_URL_HOST" "$WEB_HOST" \
     -e "CONGAME_WEB_URL_PORT" "443" \
     -e "CONGAME_WEB_URL_SCHEME" "https" \
     -e "PLTSTDERR" "error debug@GC" \
     -e "VERSION" "$GITHUB_SHA" \
     "build/web" "$GITHUB_SHA" "$TARGET_HOST"
