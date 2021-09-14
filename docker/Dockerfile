FROM fukamachi/qlot

WORKDIR /app
COPY . /app
RUN qlot install

FROM clfoundation/sbcl:slim-buster
ENV APP_ENV local
ENV CLACK_HANDLER hunchentoot

WORKDIR /app
COPY --from=0 /app/.qlot /app/.qlot

RUN set -x; \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf"

ENTRYPOINT ["/app/docker/entrypoint.sh"]
