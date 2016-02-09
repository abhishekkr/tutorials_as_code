## Thoughts on Time-series Databases

* [source](http://jmoiron.net/blog/thoughts-on-timeseries-databases/)

#### attempts for tsdb in golang community
* [catena](http://misfra.me/state-of-the-state-part-iii)
* [benchmarking boltdb](https://sacrilege.io/benchmarking-bolt/)
* [seriesly](https://github.com/dustin/seriesly)
* [influxDB](http://influxdb.com/)
* [prometheus](https://github.com/prometheus/prometheus)
* [statsd](https://codeascraft.com/2011/02/15/measure-anything-measure-everything/)

> been a popular way since 90s, [MRTG](http://oss.oetiker.ch/mrtg/doc/mrtg.en.html)

---

* [Primer TSDB Requirements](http://www.xaprb.com/blog/2014/06/08/time-series-database-requirements/)
>
> Performance and Scaling Characteristics
> A time-series database should be:
> 
> Distributed by design — no bolt-on clustering or sharding. Automatic data distribution, automatic query distribution. Fault-tolerant and highly available, with built-in replication and automatic failover. I think by this point we should all understand what it means for a database to be natively distributed. There are several good examples of databases that do it sensibly, and little of this should need to be novel.
> Send the query to the data, don’t bring the data to the query. This is a restatement of “automatic query distribution.” Queries may touch many gigabytes or terabytes of data, so moving it across the network is not scalable.
> Efficient per-node so it is capable of running at large scale without requiring thousands of servers.
> Able to take advantage of powerful hardware: PCIe flash storage, lots of RAM, many CPU cores. This rules out single-writer systems.
> Fast and consistent. No spikes or stalls; no checkpoint freezes; no compaction lock-ups.
> Operational Requirements
> I do not specifically need ACID, but I need the database to quickly recover to a consistent state after events like a power failure. For my purposes, time-series data is not subject to the same durability constraints as financial data.
> Non-blocking backups are a must. Incremental backups are a very good thing.
> It needs to be possible to scale the cluster up or down without downtime or locking.
> Compressed storage. Time-series data is big, but highly compressible.
> The database should be well instrumented.
>

---

Few basic approaches:

* Use Files (RRD, Whisper)
* Use an LSM Tree backed store (LevelDB, RocksDB, Cassandra)
* Use a B-Tree ordered k/v store (BoltDB, LMDB)

If Time Series is a vector of points, there are few primary operations

* create a new vector
* find a vector (varies)
* append to a vector (O(1))
* read from a vector (varies)

---



