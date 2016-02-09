## Building Real-Time Systems with OpenSource Technologies
> by, Gian Merlino and Fangjin Yang
> from StrangeLoop/Youtube

* dash.metamarkets.com/wikipedia_editstream/explore

* Hadoop not optimized for query latency; need a query service layer

* Make queries faster for for specific data aggregation results

```
 _________    BEFORE                          _________
 |       |--->[ Hadoop ]--------------------->|        |
 |Event  |          THEN                      |Insights|
 |Streams|--->[Hadoop         ]--->[Query]--->|        |
 |       |    [(pre-processing]    [Layer]    |        |
 ---------    [ and storage)  ]               ----------
```

> * Query Layer == RDBMS (MySQL, PgSQL)
> Scan speed of data was quiet slow.
>
> * Query Layer == NoSQL Key/Val (HBase, Cassandra)
> For Business Intelligence style queries with lots of aggregates, you often end up doing many pre-computing results.
>
> * Query Layer == Commercial (Vertica, Redshift)
> but customization on FOSS is powerful

So came up with Druid for Query Layer in (opensoruce-d in Oct 2012).
Designed for low latency ingestion and aggregation.

---

#### Design

* Truncate timestamps

Instead of storing every single raw event, tries group-by or aggregate over data dimensions/metrics.

Partitions data by time. Creates an immutable block of data for particular time segment.

* Immutable Segments
> * Read Consistency for free
> * One thread scans one segment, multiple can access same underlying data
> * Simplifies distribution: just move blocks of data around
> * Replication: just copy over a block
> * Segment size -> computation completes in milliseconds

It's a system really optimized for reads.

* Fundamentally a column store
> * scan and load what you need
> * impressive compression algorithms available
> * borrows a lot ideas from search infrastructures, can build unique indexes to make sure only required data gets scanned in a query

---

#### Architecture

```
                   /-->[Historical Nodes]<---[Broker Node]<--\
[Data]-->[Hadoop]--*-->[Historical Nodes]<*--<                -[Queries]
                   \-->[Historical Nodes]<---[Broker Node]<--/
```

---

#### What then

* Gave arbitrary data exploration & fast queries
* 90th/95th/99th % queries on >100TB data within 1s/2s/10s
* But
> * Batch loading is slow
> * Need real-time
> * Need alerts, operation monitoring, etc.

---

#### What it was

* Clients uploaded data to S3
* Hadoop+Pig to clean->transform->join it
* load result to druid
* typical turn-around in 2-8hrs

---

#### So for Real-Time Data Pipeline

* 3 obstacles
> * acquire raw-data
> * prcess raw data
> * load raw data into query engine

* Acquire Raw Data
> Need a highly efficient message queue with simple design.
> * Fast delivery with Kafka, a high-throughput event delivery service
> * It buffers incoming data to give consumer time to process, can place an HTTP API in front.

* Processing Raw Data
> * Storm, a stream processor. Processes one event a time.
> Needed Storm topologies to do same what Hadoop jobs did.
> Load opration, stream data from Kafka.
> Map operation, stream friendly (taking every event and assigning a key/val to it)
> Reduce operation can be windowed with partitioned state (take every item with same key and perform required reduction on it). Pick a key and put a timer on it, do reduce when timer runs out.

* Fast loading with DRUID
> * have an indexing system
> * real-time workers can build indexes while serving queries
> * serving system that runs queires on data

```
[Kafka    ]\   [ Kafka]  /[Storm  ] real-time  [DRUID   ]  Periodic  [DRUID     ]
[Producers]->--[Broker]->-[Workers]----------->[Realtime]----------->[Historical]
[         ]/             \[       ]            [Workers ]            [Cluster   ]
                                                       .\,                ./,
                                                        [DRUID Query Broker]
```

```
 _________    BEFORE                          _________
 |       |--->[ Hadoop ]--------------------->|        |
 |Event  |          THEN                      |Insights|
 |Streams|--->[Hadoop         ]--->[DRUID]--->|        |
 |       |               NOW                  |        |
 |Streams|--->[Kafka]-->[Storm]--->[DRUID]--->|        |
 ---------                                    ---------
```
---

**But window may be too small for accurate operations, Storm is not perfect. Hadoop was good at it.**

* Can bring back hadoop, an open-source Lambda Architecture
> * Batch processing runs for all data older than few hours
> * Batch segments replace real-time segments in DRUID
> * Query Broker merges result from both systems

```
                          Storm
                          Kafka          Tranquility
[Kafka    ]\   [ Kafka]  /----->[Storm  ]----------+-->[DRUID Real] [DRUID     ]
[Producers]->--[Broker]->                           \               [Historical]
[         ]/             \----->[Hadoop ]----------( \__ )--------> [Cluster   ]
                          Camus          Druid         .\,                ./,
                                                        [DRUID Query Broker]
```

---

References

* [Druid](http://druid.io) : @druidio
* [Storm](http://storm.incubator.apache.org) : @stormprocessor
* [Hadoop](http://hadoop.apache.org)
* [Kafka](http://kafka.apache.org)
* [RAD Stack : Realtime Analytics Data Stack](https://metamarkets.com/2014/building-a-data-pipeline)

---
---
