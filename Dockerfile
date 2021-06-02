FROM congame-source:latest

WORKDIR /src/congame
RUN raco koyo dist ++lang north

RUN cp -r /src/congame/dist /opt/congame
COPY bin/run-congame.sh /opt/congame/bin/run-congame.sh
CMD ["dumb-init", "/opt/congame/bin/run-congame.sh"]
