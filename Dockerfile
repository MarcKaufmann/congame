FROM ghcr.io/marckaufmann/racket:7.9-cs-full AS build

WORKDIR /opt/congame
COPY .git /opt/congame/.git
COPY ci /opt/congame/ci
COPY congame-core /opt/congame/congame-core
COPY congame-example-study /opt/congame/congame-example-study
COPY congame-pjb-studies /opt/congame/congame-pjb-studies
COPY congame-price-lists /opt/congame/congame-price-lists
COPY congame-web /opt/congame/congame-web
COPY migrations /opt/congame/migrations
COPY resources /opt/congame/resources
COPY static /opt/congame/static

RUN ci/setup-catalogs.sh
RUN raco pkg install -D --auto --batch \
  congame-core/ \
  congame-example-study/ \
  congame-pjb-studies/ \
  congame-price-lists/ \
  congame-web/ \
RUN raco koyo dist ++lang north


FROM ghcr.io/marckaufmann/debian:bullseye-slim

COPY --from=build /opt/congame/dist /opt/congame

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    curl \
    dumb-init \
    libargon2-1 \
    lbzip2 \
    libcairo2 \
    libdbus-glib-1-2 \
    libfontconfig1 \
    libglib2.0-0 \
    libgtk-3-0 \
    libjpeg62 \
    libpango-1.0-0 \
    libpangocairo-1.0-0 \
    libssl-dev \
    xvfb \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN curl -Lk 'https://download.mozilla.org/?product=firefox-latest-ssl&os=linux64&lang=en-US' > /tmp/firefox.tar \
    && (cd /tmp && tar -xvf firefox.tar) \
    && mv /tmp/firefox /opt/firefox \
    && ln -s /opt/firefox/firefox /usr/bin/firefox

COPY bin/run-congame.sh /opt/congame/bin/run-congame.sh
CMD ["dumb-init", "/opt/congame/bin/run-congame.sh"]
