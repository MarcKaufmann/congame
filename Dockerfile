FROM racket/racket:7.8-cs-full

WORKDIR /opt/congame
COPY .git /opt/congame/.git
COPY ci /opt/congame/ci
COPY congame /opt/congame/congame
COPY migrations /opt/congame/migrations
COPY resources /opt/congame/resources
COPY static /opt/congame/static

RUN ci/setup-catalogs.sh
RUN raco pkg install -D --auto --batch congame/
CMD ["racket", "/opt/congame/congame/dynamic.rkt"]
