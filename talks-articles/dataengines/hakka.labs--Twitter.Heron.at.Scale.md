## Twitter Heron at Scale
> source: DataEngConf 2016, Hakka Labs
> speakers: Karthik Ramasamy
[youtube:uri](https://youtu.be/YlDgslYuKcQ)

[Heron](https://twitter.github.io/heron/)
> A realtime, distributed, fault-tolerant stream processing engine from Twitter.

* `Heron` is Twitter's step-up from `Storm`, have been running in Production now(June/2016) close to 2 years.

* `Heron` also got, similar to `Storm`
> * Topology: DAG with Vertices as computation and edges as streams of data tuples
> * Spouts: Sources of data tuples for Topology (like Kafka, Kestrel, MySQL, PgSQL)
> * Bolts: Process incoming tuples and emit outgoing tuples at vertices (like filter, aggregate, join)

* When a bolt emits a tuple, which next bolt gets it depends of `Stream Grouping`
> * Shuffle Grouping, distributes randomly
> * Fields Grouping, group tuples by a/multiple field(s)
> * All Grouping, replicates tuples to all next level bolts
> * Global Grouping, sends entire stream to one next level bolt

### Why Heron? (relative to Storm)

* huge performance boost, relatively very less CPU load

* easier to debug, task isolation and profiling

* resource reservation (not in Storm), so can use common cluster

* fully API compatible with Storm

* use of mainstream languages (C++/Java/Python)

* can write engine in any language

* moved from thread-based (storm) to process based system, gives more control and isolation

---

## Heron Architecture

```
  .---------.---------->[Topology 1]
  [Scheduler]---------,.
  '---------'-----,     [Topology 2] 
     ,^    '----,  ''''\
     \          |       [Topology 3]
  (Topology  )  |           *
  (Submission)  '----,      *
                      \.    *
                        [Topology N]
```

### Topology Architecture


```
   .----------,   Logical Plan, Physical Plan, and Execution State
   [ Topology ]--------------------->.--------.
   [ Master   ]              ,------>[ ZK     ]
   '----------'  Sync Physi-/-cal    [Cluster ]
     '|'  '\_______________/_, Plan  '--------''
      |                    | \       /'
      |      ,------------/   \     /             
  .---|-----/-----------.   .--\----|-------------.
  |  .|,  ./,           |   |  ,\. .|,            |
  | [Stream-] [Metric-] |   | [Stream-] [Metric-] |
  | [Manager] [Manager] |   | [Manager] [Manager] |
  |                     |   |                     |
  | [I1] [I2] [I3] [I4] |   | [I1] [I2] [I3] [I4] | (these I1-4 are Instances)
  |_____________________|   |_____________________| (process for bolts)
        Container                 Container

```

Scheduler schedules Topology-Master as first thing for Topology.
When Topology-Master comes up in Container#0. It asks schedulers where it can spawn its containers and does so.
Stream-Manager comes up first in a container and discovers where are Topology-Master from Zookeeper.

---

## Twitter's Heron Use-Cases

* Realtime ETL(Extract-Transform-Load)
* Realtime BI(Biz Intel)
* Realtime Trends
* Realtime ML(Machine Learning)
* Realtime Media
* Realtime Ops (Metric Analytics for Infra)
* Product Safety

---

## MicroStream Engine

```
  [ State ]                                       [Scribe]  [Graphite]
  [Manager]----,                                     |    ___|
  [  SPI  ]     \                                    |   /
                 '[Toplogy] [Stream ] [========] [Metrics]
                 _[Master ] [Manager] [Instance] [Manager]
  [Scheduler]---'   |          |    __/   ________/
  [  SPI    ]       |          |   /     /
                  [Basic Inter/Intra IPC ]
                  [------HARDWARE--------]
```

* Can replace components, rewrite.
* Support multi-language API with native instances

---

## Heron Environment

#### Laptop/Server

* Local Scheduler
* Local State Manager
* Local Uploader

#### Cluster/Testing

* Mesos/Aurora Scheduler
* ZK State Manager
* HDFS Uploader

#### Cluster/Production

* Aurora Scheduler (use Aurora Specifics)
* ZK State Manager
* Packer (internal versioned job uplader) Uploader

---

## Why Aurora?

* resource quotas enforced
* authorization
* expressive DSL for jobs
* HA, Scalability
* In built monitoring and debugging

---

## Stragglers

> are norm in a multi-tenant distributed systems.

* Bad Host
> in a large cluster, some host goes bad but before that something gets scheduled for it

* Execution Skew
> hot key going to a single process but not getting handled

* Inadequate Provisioning
> lag for fewer resource than required

### Approaches to handle

* senders to straggler drop data
> it's unpredicatable, bad for accuracy
> no visibility on where and why data is dropped

* senders slow down to speed of straggler, back pressure; **used in Heron**
> predictable, processes data at maximum possible rate
> reduce recovery times
> handles temporary spikes
> sustained back pressures are mostly cuz of `irrecoverable GC cycles` or `faulty host`

* detect straggler and rescehdule them

> Have another topology just to detect faulty hosts.

### Load Shedding

* sampling based apporach
> down sample incoming stream and scale up results
> reasonable for uniform sample, hard to achieve uniformity across spouts

* drop based apporach
> simply drop old data, spouts take lag-threshold and adjustment value
> more practical

---

## Heron Resource Usage

following observation is based on an experimental setup

#### Resource Consumption

* spout-instances 84%
* bolt-instances   9%
* heron-overhead   7%

#### Profiling Spouts

* deserialize     63%
* kafka-iterator  16%
* kafka-fethc      7%
* mapping          6%
* parse-filter     6%
* REST             2%

#### Profiling Bolts

* persist-data    68%
* serialize       19%
* aggregation      5%
* data-transport   4%
* REST             2%
* deserialize      2%

#### Overall

* Fetching Data   60%
* User Logic      21%
* Heron Usage     11%
* Writing Data     8%

---
---
