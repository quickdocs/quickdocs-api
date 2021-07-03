# quickdocs-api

Quickdocs.org API server

## Requirements

* Docker Engine
  * [Install Docker Engine | Docker Documentation](https://docs.docker.com/engine/install/)
* Docker Compose
  * [Install Docker Compose | Docker Documentation](https://docs.docker.com/compose/install/)

## Usage

```
$ docker-compose up

# Run after the first run to initialize DB
$ docker run --rm -it --net=host -e DB_PORT=25432 quickdocs/dist-updater setup
$ docker run --rm -it --net=host -e DB_PORT=25432 quickdocs/dist-updater update
```

## See also

* [quickdocs/dist-updater](https://github.com/quickdocs/dist-updater), DB table definitions
* [fukamachi/utopian](https://github.com/fukamachi/utopian), a web framework
* [fukamachi/mito](https://github.com/fukamachi/mito), an O/R mapper

## License

Copyright (c) 2021 Eitaro Fukamachi

Licensed under the BSD 3-Clause License.
