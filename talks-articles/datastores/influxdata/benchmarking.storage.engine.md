
### Benchmarking InfluxDB Storage Engines: v0.10, v0.9 and v0.8

> by Todd Persen

got new storage engine in 2016

#### The Time-Structured Merge Tree

* Columnar Storage, unlimited fields

* Adaptive compression depending on data-types for both timestamps and values

* Compactions in background, creating larger TSM files for optimal compressions

* Built-in WAL, with query-able in-mem cache

* Each series uniquely indexed

---

#### How to Test a Storage Engine

* accuracy
* throughput
* compression
* memory footprint
* failure testing
* scale testing

---

#### v0.8.x

* relied on 3rd party storage engine
> leveldb to hyperleveldb to rocksdb to lmdb; all written in C/C++

* no secondary indexing capabilities

* limited customizability

```
          [writes points/sec]  [reads points/sec] [int64 sotrage bytes/point] [arch]
LevelDB:    45,000               550,000            27                         LSM
RocksDB:    75,000               600,000            32                         LSM
LMDB   :    25,000               300,000            76                         B+
```

---

#### v0.9.x

* based on BoltDB, in Go

* tags supported natively as indices

* 2 variants
> b1: pure boltdb, timestamp as a key with single value
> bz1: boltdb with compressed blocks, first timestamp in block as key

* demanding on IOPS

* about 30bytes/point

* didn't much exceed v0.8.x perf

---

#### v0.10.x

* TSM Testing and Tooling
> * `influx_stress`; over VMs and physical machines
> * testing retention policies, queires, deletes during writes
> * on SSD, a day and a half for 50Billion points (170Gigs on disk) through a single node (with no retention policy kicking in)

So,
* over 350K writes/sec with 2 bytes per point on disk
* reads 300K points/sec increasing to 2M/sec with new query engine
* crash stable; steady IOPS under full write load
* sample 100GB dataset starts in `<60seconds`, indices fully-loaded

---

#### Next

* write performance degrades with very high series cardinality (`>10M` unique series)
> * move indices for cold shards out of memory
> * experiemtns with alternative index structure

* still CPU bound

* dictionay index for strings

* better query engine integration in v0.11.0

---

#### QnA

* WAL does `fsync` after every write

---
---
