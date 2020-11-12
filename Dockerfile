FROM racket/racket:7.9-cs-full AS build

WORKDIR /opt/congame
COPY .git /opt/congame/.git
COPY ci /opt/congame/ci
COPY congame /opt/congame/congame
COPY migrations /opt/congame/migrations
COPY resources /opt/congame/resources
COPY static /opt/congame/static

RUN ci/setup-catalogs.sh
RUN raco pkg install -D --auto --batch congame/
RUN raco koyo dist ++lang north


FROM debian:bullseye-slim

COPY --from=build /opt/congame/dist /opt/congame

CMD ["/opt/congame/bin/congame"]
