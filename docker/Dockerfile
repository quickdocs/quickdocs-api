FROM fukamachi/qlot

WORKDIR /app

COPY qlfile /app
COPY qlfile.lock /app

RUN set -x; \
  qlot install --no-deps

FROM fukamachi/sbcl
ENV APP_ENV local
ENV CLACK_HANDLER hunchentoot

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends \
    libyaml-dev && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=0 /app/.qlot /app/.qlot

RUN set -x; \
  ros -l .qlot/setup.lisp -e '(ql:quickload :clack)'

ENTRYPOINT ["/app/docker/entrypoint.sh"]
