# quickdocs-api

Quickdocs.org API server

## Requirements

* Docker Engine

## Usage

```
$ docker-compose up

# Run after the first run to initialize DB
$ docker run --rm -it --net=host -e DB_PORT=25432 quickdocs/dist-updater setup
$ docker run --rm -it --net=host -e DB_PORT=25432 quickdocs/dist-updater update 2021-04-21
```

## License

Copyright (c) 2021 Eitaro Fukamachi

Licensed under the BSD 3-Clause License.
