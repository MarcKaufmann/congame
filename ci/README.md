# Deployment recommendation

## Postgres

You can setup a standard postgres instance on your host, or run postgres in a docker container.

### Postgres docker container with data volume

First you have to set up a network for the docker containers to be able to talk to each other. Here we use the name `congame` for the network:

`$ docker network create congame`

Next, we need to create a data volume where the data will be written to, so that it persists when the postgres container is removed. Here we call it `pgdata-13`:

`$ docker volume create pgdata-13`

Then you have to set up the postgres container connected to this network and mount the data volume:

```
$ docker pull postgres:13
$ docker run --name postgres-13 --network congame -e POSTGRES_PASSWORD=<some-password> -d -v pgdata-13:/var/lib/postgresql/data postgres:13
```

We should check the container runs:

``` shell
$ docker exec -it postgres-13 bash
$ su postgres
$ psql
```

and we are in a psql session inside the docker container where we can connect to existing databases, create new ones, etc.

If the docker container gets stopped, all you need to do is to run `docker start postgres-13`.

### Backup recommendations

To avoid losing data, we can back it up via cron:

``` shell
# Make one daily db dumps for teaching, which works when not running a study -- change to hourly when running a study.
47 11 * * * docker exec postgres-13 /usr/bin/pg_dump -Fc -U congame -h localhost > $HOME/backups/congame/congame-`date +\%Y-\%m-\%d-\%H-\%M-\%S`.dump 2>&1
# Make 2 db dumps of identity, less can go wrong. Store as identity-congame to make backups to remote storage easier below
48 11,17 * * * docker exec postgres-13 /usr/bin/pg_dump -Fc -U congame_identity -h localhost > $HOME/backups/congame/identity-congame-`date +\%Y-\%m-\%d-\%H-\%M-\%S`.dump 2>&1
```

This backs up the data on the server, and thus it should be backed up on another machine to avoid data loss in case the server goes down or we lose access to it.

