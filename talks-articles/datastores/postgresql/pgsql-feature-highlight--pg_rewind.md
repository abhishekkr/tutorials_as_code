
## PostgreSQL feature highlight: `pg_rewind`

> switch slave to master easily

`pg_rewind` resynchronizes a cluster with another copy of cluster

```
mkdir bob eve
chmod 0700 bob eve


initdb -D bob

vim bob/postgresql.conf
# port = 5432
# wal_level = hot_standby
# wal_log_hints = on
# checkpoint_segments = 64
# max_wal_senders = 2
# max_wal_keep_segments = 64
# hot_standby = on
# log_destination = 'stderr'
# logging_collector = on
# log_directory = 'pg_log'
# log_filename = 'postgresql-%a.log'


vim bob/pg_hba.conf
# local     all         all                       trust
# host      all         all        127.0.0.1/32   trust
# host      replication postgres   127.0.0.1/32   trust


pg_ctl -D bob start


pg_basebackup -PR -X stream -c fast -h 127.0.0.1 -U postgres -D eve/
# do same for 'eve' but with different port like '5433'

vim eve/recovery.conf
# standby_mode = 'on'
# primary_connection = 'user=postgres host=127.0.0.1 port 5432 sslmode=disable sslcompression=1'
# trigger_file = 'failover'


pg_ctl -D eve start


psql -p 5432
# create database db1;


pgbench -i -s 2 -U postgres db1

## check if synced on -p 5433


touch eve/failover


pgctl -D bob/ stop


pgbench -i -s 2 -U postgres -p 5433 db1


pg_rewind -D bob/ --source-server="host=127.0.0.1 port=5433 user=postgres"
## changed port copied over from eve back to 5432
## rm bob/recovery.done
## rm bob/backup_label*
## mv eve/recovery.done bob/recovery.conf

vim bob/recovery.conf
# standby_mode = 'on'
# primary_connection = 'user=postgres host=127.0.0.1 port 5433 sslmode=disable sslcompression=1'
# trigger_file = 'failover'

#
#
```

---
