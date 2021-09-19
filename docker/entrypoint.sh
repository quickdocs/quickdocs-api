#!/bin/bash

PORT=${PORT:-5000}
CLACK_HANDLER=${CLACK_HANDLER:-hunchentoot}

exec .qlot/bin/clackup -S . -s quickdocs-api --server $CLACK_HANDLER --address 0.0.0.0 --port $PORT --debug nil app.lisp
