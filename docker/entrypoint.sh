#!/bin/bash

PORT=${PORT:-5000}
CLACK_HANDLER=${CLACK_HANDLER:-hunchentoot}

exec sbcl --noinform --non-interactive --load .qlot/setup.lisp \
  --eval '(ql:quickload (list :clack :quickdocs-api))' \
  --eval "(clack:clackup #P\"app.lisp\" :address \"0.0.0.0\" :port $PORT :server :$CLACK_HANDLER :use-thread nil :debug nil)"
