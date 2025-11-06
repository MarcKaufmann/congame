# Deployment recommendation

## The apps

See `.github/workflows/ci.yaml` and `deploy.sh`.

## Postgres

Install a recent enough (13+) version of Postgres your distribution's
package manager.

### Backup recommendations

To avoid losing data, we can back it up via cron:

``` shell
# Make one daily db dumps for teaching, which works when not running a study -- change to hourly when running a study.
47 11 * * * pg_dump -Fc -U congame -h localhost > $HOME/backups/congame/congame-`date +\%Y-\%m-\%d-\%H-\%M-\%S`.dump 2>&1
# Make 2 db dumps of identity, less can go wrong. Store as identity-congame to make backups to remote storage easier below
48 11,17 * * * pg_dump -Fc -U congame_identity -h localhost > $HOME/backups/congame/identity-congame-`date +\%Y-\%m-\%d-\%H-\%M-\%S`.dump 2>&1
```

This backs up the data on the server, and thus it should be backed up
on another machine to avoid data loss in case the server goes down or
we lose access to it.
