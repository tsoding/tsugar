# Tsugar

## Quick Start

```console
$ nix-shell    # for NixOS only
$ cabal build
$ cabal run config.json
$ <browser> http://localhost:3000/
```

### config.json

| field      | description             |
|------------|-------------------------|
| `pgUrl`    | PostgreSQL URL          |
| `httpPort` | HTTP Port of the server |

### PostgreSQL

The easiest way to setup PostgreSQL for development is to user docker:

```console
$ docker run --name some-postgres -p 5432:5432 -e POSTGRES_PASSWORD=mysecretpassword -d postgres
$ docker run -it --rm --link some-postgres:postgres postgres psql -h postgres -U postgres
```

Use `postgres://postgres:mysecretpassword@localhost:5432/postgres` as `pgUrl` in [config.json](#config.json).
