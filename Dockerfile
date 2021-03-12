FROM ghcr.io/marckaufmann/racket:7.9-cs-full AS build

WORKDIR /opt/congame
COPY .git /opt/congame/.git
COPY ci /opt/congame/ci
COPY congame /opt/congame/congame
COPY congame-core /opt/congame/congame-core
COPY study-tools /opt/congame/study-tools
COPY congame-example-study /opt/congame/congame-example-study
COPY congame-pjb-studies /opt/congame/congame-pjb-studies
COPY congame-price-lists /opt/congame/congame-price-lists
COPY migrations /opt/congame/migrations
COPY resources /opt/congame/resources
COPY static /opt/congame/static

RUN ci/setup-catalogs.sh
RUN raco pkg install -D --auto --batch \
  congame-core/ \
  study-tools/ \
  congame-example-study/ \
  congame-pjb-studies/ \
  congame-price-lists/ \
  congame/
RUN raco koyo dist ++lang north


FROM ghcr.io/marckaufmann/debian:bullseye-slim

COPY --from=build /opt/congame/dist /opt/congame

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    curl \
    dumb-init \
    libargon2-1 \
    libssl-dev \
    libglib2.0-0 \
    libfontconfig1 \
    libcairo2 \
    libpango-1.0-0 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

CMD ["dumb-init", "/opt/congame/bin/congame"]
