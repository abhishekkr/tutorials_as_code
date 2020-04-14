
## Apache Pulsar - The next generation messaging system

> by Karthik Ramasamy, Streamlio (March 21, 2018)
>
> [source: Linkedin Engineering Youtube Channel](https://www.youtube.com/watch?v=lAVPIY1PRdo)

### What is Apache Pulsar

* a flexible pub-sub system backed by a durable log storage

* Highly Durable, data is replicated and synced to disk. Two layer architecture.

* Guaranteed Ordering within a partition.

* Delivery Guarantees; with at least once, exactly once & effectively once to avoid deduplication.

* Geo Replication, out of the box.

* Multi-tenancy, single cluster can support many tenants. Allows isolation, guaranteed resources, more.

* Low-latency, 5ms at 99%.

* Unified messaging model, supports both topic and queue semantic in single model.

* High Throughput, can reach 1.8M  messages/sec in single partition.

* Highly Scalable, can support millions of topics in single cluster.

---

### Archutecture Basics

* 2 layer architecture. Brokers are serving nodes and Bookies (Apache BookKeeper) are storage nodes.

* Each layer can be scaled independently.

* No data locality, data for single topic/partition is not tied to a particular node.

* Automatic Load Balancing among Brokers.

#### Pulsar Broker

* Only point for client interaction, acquires ownership for topic groups & "serve" them.

* Not durable as no data at this layer, client uses service discovery mechanism to auto migrate to working broker.

* Can explicitly configure for data retention at broker if use-case requires it.

#### Segment Centric Storage

* Storage for a topic is an infinite stream of messages, called ledgers in BookKeeper.

* Implemented as sequence of segments; each segment is a replicated log (BookKeeper ledger).

* Segments are rolled over based on time, size and after crash.

* Broker Failure: Auto segment creation on available broker on a broker failure, client library handles connection by itself.

* Bookie Failure: On write failure, immediately switches write to a new bookie within same segment. Writes can continue until there are any 3 bookies in a cluster.

* In background, starts a many-tomany recovery process to regain configured replication factor.

* Seamless Cluster Expansion, can just add a new bookie or broker without hassle.

> Kafka is a Partition Centric distribution where one partition is tied to a node. Each partition has a leader and is replicated. Data gets to leader of broker and is replicated. Retention is also limited by single node's capacity. Recovery required `rebalancing` which impacts traffic.
>
> Pulsar has no single partition, but smaller segments which are spread all over the place as resource balance and durability gets calculated.

#### Flexible Message Model

* Provides exclusive and failover subscription; also can ave shared consumer getting a portion of traffic

#### Multi-Tenancy

* Authentication/Authorization/Namespaces/Admin-APIs

* I/O Isolations between reads & writes (provided by BookKeeper)

* Soft Isolation; each tenant can have quota on storage with flow-control, back-pressure, rate-limiting

* Hardware Isolation; constrain tenants on subset of brokers or bookies

#### Geo-Replication

* Simple config to add/remove regions for availing scalable async replication, integrated in broker message flow

#### Client Library

* Java, C++, Python, WebSocket APIs

* Partitioned topics; Kafka compatible wrapper API; transparent message batching, compressiong, E2E encryption, auth

#### Pulsar Functions

* ETL

* Reactive Services

* Classification

* Real-time Aggregation

* Event Routing

* Microservices

* SDK Less for Java & Python

> able to hold more than double message rate than Kafka

#### Use Cases

* Message Queue

```
   (HTTPService)-----------,                     ,--->[ComputA]----,
                            :---->[Pulsar Topic X]----->[ComputeB]-->[Pulsar Topic Y]-->{DB}
   (HTTPService)-----------'                     '--->[ComputeC]---'      |
       ^\                                                                 |,
         '--------------------------------------------------------->[[Cache Serving]]
```

* Microservices

> Delegate processing to multiple compute jobs which interact with Microservices. Wait until response is pushed back to HTTP Server.

* Notifications

> Replicating DB transactions across regions, notification to multiple interested tenants.

* Feedback System; co-ordinate a large number of machines and propogate state.

---

### Apache BookKeeper

* a replicated distributed log storage

* provides low-latency durable writes, simple repeatable read consistency, highly available, store many segments per node and I/O isolation

* a single bookie can serve & store hundreds of thousands of ledgers

* write and read paths are separated to avoid read impact write latency

> * writes added to in-mem write-cache & committed to journal
>
> * write-cache flushed in background to device

* entries are sorted, mostly sequential reads, segments can be located via index and fetched separate

---

### Pulsar in Production

* at Yahoo!, Comcast

* 3+ years; serving 2.3 million topics; 100billion messages/day; average latency < 5ms

* 99% 15ms (strong durable guarantee); 0 data loss; 80+ apps; self-served provisioning

* full mesh cross-datacenter replication, 8+ datacenters

---
