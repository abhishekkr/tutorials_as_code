
## The proper care and feeding of a MySQL Server for Busy Linux Admins
> LinuxFest NorthWest 2016
> Dave Stoker, @stoker, [site](http://opensourcedba.wordpress.com)

* Hardware
* Software
* Backup, Replication
* Tools to make life easier
* Configurations

### Hardware

* they love memory, reading from memory is 100,000 times faster than from disk

* move data to separate controller/disk from logs, don't log on slow devices

* RAID to your fav level - RAID 10 minimum

* don't use consumer grade disks

* Caches, disks and controllers, write through or write back caches
> both lie, make sure they don't auto tune during prod hours

* FusionIO cards
> atomic writes, no double buffering, thus speed

#### Network

* never make public

* scrub rigourously

* keep separate net for replication/backup

* MySQL auth to use `host`, `user` and `password`.
> Configure server to use IP addresses instead of names.
> Bad DNS zone transfers shouldn't bring down database access.

#### Slave Servers

* Slave need bigger/badder hardware than maser, do more work.

* Use MySQL utilites to clone masters, set-up slaves

* Dedicated network

---

### Software

* Run latest version available

* Just MySQL on node.

* No Swapping.

* May be a caching layer but watch memory use.

---

### Backups

* as often backups as they seem too many

* restore-able instances, DBs, tables, ...

* off-site backups

* random restore tests

---

### Replication

* easy to set-up and misunderstand

* 2 types
> * Async: slave grabs a copy of changes from master and apply to self, master unaware of slave
> * Semi-sync: master waits for ACK from at least one slave before procedding

* 3 forms: Statement, Row and Mixed.

* Single-threaded before 5.6

* Multi-threaded for different DBs in 5.6

* Multi-infra-DBs threaded for 5.7


#### Replication Filters

* Don't need to replicate everything
> check churn of data, maybe 1x day backup

* Filter Tables

* Can change filters on fly with 5.7
> 'something going on in manufacturing, cab we capture it someplace'


#### Global Transaction IDs

* since `5.6` each transaction has unique GTID

* storing replication data in InnoDB tables plus adding checksums make crash safe

* row based can exploit only sending keys and changed items, not entire row of data


#### Multimaster and Multisource

* Multi-Master
> **Not Recommended**
> System.A auto\_increment odd numbers and
> System.B auto\_increment even numbers
> need to be watched

* Multi-Source
> **MySQL 5.7**
> Multiple masters send data to one slave for master backup.
> Make sure sharded data doesn't overlap.


#### Group Replication MySQL 5.7

> Galera


#### Replication for backup

* Replication uses 3 threads
> Master to Slave
> Slave to Log
> Log to Data

* On a small shop DB
> Keep master running
> shutdown log to data thread
> run backup
> restart log to data
> Slave will catch-up

---

### Tools to make life easier

* Monitoring
> VividCortex (if wanna pay)
> Nagios, InfluxDB, Prometheus

* MySQL Workbench
> help on optimizing query
> users, backup, imports
> Dashboard, SYS Schema
> Entity Relationship Mapper
> Migration Tool

* MySQL utilities
> OpenSource written in Python
> Setup replication and automatic failover
> Copy user settings
> Copy data
> Look for bad proceesed and killem
> Move binary logs
> Grep for a column name in a schema
> more...

* Percoan Toolkit

* Toad for MySQL from Dell

---

### Config Suggestions

* turn off DNS lookups, zone transfer dies
> use skip-name-resolve

* save/load stats, to help optimize feature queries
> * use `innodb_stats_persistent`
> * see `14.13.16.1 Configuring Persistent Optimizer Statistics Parameters` in MySQL manual
> * `innodb_buffer_pool_dump=ON`
> * `innodb_buffer_pool_dump_at_shutdown=ON`
> * `innodb_buffer_pool_load_at_startup=ON`

* tune log level (v5.7)
> `log_error_verbosity` - errors, errors and warnings, E&W + notes
> send to SYSLOG

* turn of Query cache (v5.7 default)
> single threaded, use memcached/redis
> free up memory

* innoDB buffer pool size
> 75-80% of RAM


#### Big.Hint.1

Be damn stingy with permissions and grants.
Easier to say no than constantly being restoring.

> `--safe-updates` or `--i-am-a-dummy`, no more oops


#### Big.Hint.2

Use `Sys_Schema`
> * Whose hogging resources
> * Indexes not being used
> * Problematic Queries
> Other routine PITAs


#### Big.Hint.3

v5.7 Security
> * Security install becomes deault, forced root password, no anonymous acount
> * Password Rotation
> * Configure Rules
> * `mysql_config_editor` (v5.6.6), stores encrypted auth-creds, use `mysql --login-path=finance`

---

**Chapter.6 of MySQL manual consists most of this.**

---
---
