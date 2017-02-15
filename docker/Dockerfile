FROM alpine:latest

RUN echo "@testing http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
  && apk update \
  && apk upgrade \
  && apk add \
    curl \
    ghc@testing \
    git \
    musl-dev \
    ncurses-dev \
  && curl -sSL https://get.haskellstack.org/ | sh 2>&1 \
  && adduser -S viewprof

USER viewprof
ENV PATH="/home/viewprof/.local/bin:$PATH"
WORKDIR /home/viewprof

RUN stack config set system-ghc --global true 2>&1 \
  && stack install viewprof 2>&1

ENTRYPOINT ["viewprof"]
CMD ["--help"]
