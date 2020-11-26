FROM ghcr.io/marckaufmann/racket:7.9-cs-full AS build

WORKDIR /opt/congame
COPY .git /opt/congame/.git
COPY ci /opt/congame/ci
COPY congame /opt/congame/congame
COPY congame-core /opt/congame/congame-core
COPY congame-example-study /opt/congame/congame-example-study
COPY congame-pjb-studies /opt/congame/congame-pjb-studies
COPY migrations /opt/congame/migrations
COPY resources /opt/congame/resources
COPY static /opt/congame/static

RUN ci/setup-catalogs.sh
RUN raco pkg install -D --auto --batch \
  congame-core/ \
  congame-example-study/ \
  congame-pjb-studies/ \
  congame/
RUN raco koyo dist ++lang north


FROM ghcr.io/marckaufmann/debian:bullseye-slim

COPY --from=build /opt/congame/dist /opt/congame

RUN apt-get update \
  && apt-get install -y --no-install-recommends dumb-init libargon2-1 libssl-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

CMD ["dumb-init", "/opt/congame/bin/congame"]
