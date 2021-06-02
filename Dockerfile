FROM ghcr.io/marckaufmann/racket:8.1-full AS build

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
  congame-web/
RUN raco koyo dist ++lang north


FROM ghcr.io/marckaufmann/congame-base:latest

COPY --from=build /opt/congame/dist /opt/congame

COPY bin/run-congame.sh /opt/congame/bin/run-congame.sh
CMD ["dumb-init", "/opt/congame/bin/run-congame.sh"]
