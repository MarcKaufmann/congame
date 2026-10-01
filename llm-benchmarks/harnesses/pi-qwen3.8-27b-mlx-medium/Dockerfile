ARG BASE_IMAGE=congame-congame:latest
FROM ${BASE_IMAGE}

ARG TARGETARCH
ARG NODE_VERSION=22.19.0
ARG PI_VERSION=0.84.2

RUN apt-get update \
  && apt-get install -y --no-install-recommends git \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN case "$TARGETARCH" in \
      amd64) node_arch=x64 ;; \
      arm64) node_arch=arm64 ;; \
      *) echo "unsupported Node architecture: $TARGETARCH" >&2; exit 1 ;; \
    esac \
  && curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-${node_arch}.tar.gz" \
       -o /tmp/node.tar.gz \
  && mkdir /opt/node \
  && tar -xzf /tmp/node.tar.gz --strip-components=1 -C /opt/node \
  && rm /tmp/node.tar.gz

ENV PATH="/opt/racket/bin:/opt/node/bin:${PATH}"

COPY congame-cli /opt/congame/congame-cli
RUN PLTUSERHOME=/tmp/racket-build-home \
    /opt/racket/bin/raco pkg install --installation --auto -D \
      /opt/congame/conscript \
      /opt/congame/congame-core \
      /opt/congame/congame-cli \
  && rm -rf /tmp/racket-build-home

RUN npm install --global @earendil-works/pi-coding-agent@${PI_VERSION}

RUN ln -sf /opt/node/bin/node /usr/local/bin/node \
  && ln -sf /opt/node/bin/npm /usr/local/bin/npm \
  && ln -sf /opt/node/bin/npx /usr/local/bin/npx \
  && ln -sf /opt/node/bin/pi /usr/local/bin/pi \
  && ln -sf /opt/racket/bin/racket /usr/local/bin/racket \
  && ln -sf /opt/racket/bin/raco /usr/local/bin/raco

WORKDIR /workspace
