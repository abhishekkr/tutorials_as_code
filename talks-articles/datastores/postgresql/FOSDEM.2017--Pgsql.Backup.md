
## An Overview of PostgreSQL's backup, archiving and replication

> by Gunnar "Nick" Bluth
> at FOSDEM 2017 (H.1309)

### pg\_dump

* `pg_dump[all]` can restore state of DB at the moment initiation
> * `pg_dumpall` can dump whole clusters, dbs, tables
> * can provide textual(sql) representation or custom format

* textual format of `pg_dump` uses `COPY` for performance

* custom format `pg_dump -Fc`, restored using `pg_restore` into `psql` or straight to db
> * can restore single tables, compressed by default

* directory format `pg_dump -Fd`
> * can backup and restore in parallel `-j X`
> * can restore single tables, compressed by default

* global objects (roles, tablespaces) are not saved as `pg_dump` reads from dbs
> so do `pg_dumpall --globals-only` alongwith it


#### RTO and RPO of logical backup

RTO (recovery time operation), between minutes to days given db size.

RPO (recovery point operation), last backup run (worst case the backup before).


#### pros and cons

* backup is readable by humans, at least can be made so

* schema and roles can go to SCM

* can backup/restore single entities if need be

* reveal issues with *dark corners* of DB (data checksum)


#### way beyond `pg_dump`

* `7.1` added WAL

* `8.0` added ability for `on-line backup`, `PITR`

* `9.1` added `pg_basebackup`, wrapping existing backup methods

* `9.2` allowed `pg_basebackup` to also fetch WAL data

---

### Options

*  to get that Snapshot
> * (don't) LVM/filesystem snapshot
> * (don't) rsync
> * (yes) `pg_basebackup`

* to get WAL segments
> * `archive_command` (postgresql.conf)
> * `pg_basebackup` (with `--xlog-method=[fetch|stream]`); `-X` is also handy to clone a slave

#### Why a WAL Archive

WAL segments together with snapshot of your HEAP allow to restore DB to any point, PITR.

Need 2 things for that
* binary snapshot of your HEAP
* all WAL segments between your snapshot and your mistake

Restore target will be fetching from WAL archive, can have running all time to a Warm Standby server.

**RTO goes down to minutes, in case of Warm standby.**

**RPO to your last archived segment.**

---

### Binary Streaming Replication

> Warm Stnadby server on steroids

* WAL segments can be sent over network directly and can be replayed immediately.

* Streaming replication can be sync or async
> * choose per transaction
> * choose between `remote_write` and `remote_apply`

* Can use replication slots

* Can be cascaded

* Slaves can serve RO, can take backup from slave

* Streaming Slave can be delayed

```

  [Master]
   |   \_____________
   | WAL stream      \,
   |,               [Slave1]
 [Slave3]             |,WAL stream
                    [Slave2]

```

#### Sync Replication Pitfalls

* can have N sync slaves

* make sure always have N+1 slaves in total
> not finish any transaction before come back to N+1

* network latency becomes an issue
> choose wisely which transaction be sync and where to put sync slaves


#### Pros and Cons

Cons
> * works on whole DB cluster only
> * implication on n/w connection loss

Pros
> * `1:1` copy of your DB online
> * reliable and battle proven
> * RTO and RPO really good

---

**REPLICATION doesn't replace backup.**
**RAID doesn't replace backup.**
**SAN doesn't replace backup.**
**CLOUD doesn't replace backup.**


### Putting it all together

* need WAL archive
* need replication slaves
* minimal RTO
* RPO (closest possible - sync slave),(closest feasible - async slave)
* protection aganst human error
* RO on slave

```

               [master]---stream------------>[slave]
                  \                           /'
              archive_command    restore_command (optional for bad times)
                    \             /
  ,------------------\-----------/--------------------,
  | [1:1 copy]        \,        /                     |
  | [of      ]      [WAL Archive]                     |
  | [PGData  ]                                        |
  '---------------------------------------------------'

```

#### Pros and Cons

Cons
> * major version has to be the same

Pros
> * all of replication
> * all of WAL archive

---

### Configure PostgreSQL.conf

```
wal_level               = replica       ## or logical already
archive_mode            = on            ## always to cascade
archive_command         = /some/archive/script %p %f
max_wal_senders         = 10            ## or more
synchronous_commit      = local         ## for now
synchronous_standby_names   =   '' | <set>
hot_standby             = on
log_collector           = on
```

---

### To Setup your WAL Archive

* Use `pgbarman`, `pgbackrest`, `WAL-E`

> not to keep on your DB node, not even same DataCenter


#### `/your/archive_script.sh`

* only slightly complex functionality will not fit in `archive_command`

* script can be changed without HUPing the DB

* purpose of script, somehow get `%p` ($1) to your WAL archive as `%f` ($2)

* `rsync` not bad choice, although
> * make sure `%f` doesn't exists before start sending
> * call sync remotely (or mount archive sync) after seding
> * rsync tends to give ReturnCodes `>127` when not able to resolve, filter these
> * make sure never returns ReturnCode 0 without having the job done, `set -e`, errors will end up in PG's log when turn `log_collector` on

* `rsync` means writing in some OS pagecache so `sync`; NFS is async so even more dangerous
> backup is not safe until it has been flushed to persistent storage in a safe location


#### activate archiving

* monitor it

* PG will not throw away WAL segments it could not archive, PGDATA can run out of disk space

* Replication slots have same implication

---

### Try a full Backup

using a tool anyway (e.g. barman)

#### doing first slave

* add a replication line to your master's `pg_hba.conf`

* prepare new PGDATA
> eg.g on Debian/Ubuntu do a `pg_createcluster` and `rm -rf` the result
> make sure `postgresql.conf` matchs your master's

* run `pg_basebackup -X stream -h <master> -U <user> -R -D <new-pgdata>`
> `-R` creates a `recovery.conf`

* add a `restore_command` to resulting `recovery.conf`, to get segment from your archive

* start slave, rinse-repeat


#### start looking for software

* repmgr
>  FOSS, 5yrs old or so

* PAF
> resource for Pacemaker, better than default pgsql resource of Pacemaker

* pglookout
> check availability

---

### Logical Replication

* recent past options... Slony, Bucardo, Skytools; we're all gonna die

* coming into core with 10.0

* already available with tools like `pglogical`

* if you can afford a few extra MB, already set `wal_level = logical`

* allows for
> painless, low-downtime version upgrades
> sharding
> collecting data from different DBs in a DataWareHouse
> multi-master

---

* test your backup, regressively test your retore

* make sure configs are in sync

* monitor logs and lags

---
---
