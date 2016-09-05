
## At the Heart of a Giant : Postgres at TripAdvisor
> by Matthew Kelly
> at PGConf US 2015

#### How do we Scale, Why do we

load
* 200mil reviews
* 4.5mil locations
* 29mil photos 
* 70mil email-able members
* 315mil unique visitors
* 25% load comes from crawlers

---

#### Setup

* Mostly 9.1, then some 9.2 and 9.3
> upgrading all to 9.3

* HA is currently DRBD
> getting rid of DRBD, moving to Physical Streaming Replication

* Between datacenter replication done with homegrown trigger based solution
> inspired from `DBMirror`

```
  [160 Java WebServers]------------>[4-5 RO DBs]
     | | | |        \ \
     | | | |         '-'----------->[RW Misc DB]
     |,|,|,|,
  ---ServiceTier--most-have-independent-DBs------
  

```

* Real H/w

* RAM instead of SSDs

* workloads extremely read-heavy, use of memcached and in-mem stores heavily to insulate DBs

---

#### File Backed Results Sets (FBRS)

* Used for things app needs to hold in memory (like skeletal structure of all locations in world)

* Prevents 50-60 servers from issuing same costly startup query

* relatively lightweight, just rsynced with code

---

#### Dropping Tables and Deleting Files

* Certain ops in kernel buffer cache are slow, try following experiment
> * get a machine with 20+GB RAM
> * open htop
> * create a file as big as main memory (use `dd`, whatever)
> * call `sync` to ensure no dirty-pages in cache
> * wait at least 30sec, or whatever kernel bg writer is set to
> * `rm` should block when called, watch `htop` mem usage drop and single core dropped
> repeat same steps but with dropping kernel cache before `rm`
> here dropping kernel cache shall take similar time as `rm` before, `rm` should be instantaneous

---

#### Replication

Stuck with mix of physical and trigger-based replication until logical is completed

* re-master frequently

* immediate failback is requirement (timeline switching is risky)

* trigger replication allows for 'No Down Tme' upgrades

---

#### Releases

* 10 code releases a week, 6 schema changes a week
> subset of table mods can be live
> rest done on secondary site pre failover
> automated schema validation between masters and slaves

---

#### Cause of Perf issues

* Check power settings, newer linux kernels ignore BIOS power settings
> cycle CPU down, increasing query time

* creating/destroying connections is costly
> most perf issues inflicted by connection pool

* Dynamic SQL was a huge part
> parse and plan dominated query execution of sub-millisec queries
> adding `prepared statement` caching in JDBC wrapper makes huge difference
> `prepared statements` provide huge network b/w saving even over stored proc

* PgBouncer massively helps with stability and capacity
> but only if you can rewrite all DB access to stored proc
> as it breaks `prepared statements`, as of now

---

#### Throw Hardware at it

Mostly 256-768GB RAM PgSQL 9.1 server capable of doing 50K+ QPS, fixing perf issues.

* SSDs
> SSDs RAID10 writes jump between 4Gbps to 20Gbps.
> 75K-100K fsyncs/sec on a single WAL
> 9.4 server and async commit able to write 300-400MBps of WAL

---

They dump logs in Postgres as well.


---
---
