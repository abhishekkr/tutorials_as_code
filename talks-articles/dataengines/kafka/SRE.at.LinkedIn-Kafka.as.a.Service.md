
## Kafka As A Service
##### SRE at LinkedIn, Enterprise Kafka
> by Todd Palino, Clark Haskins
> at HakkaLabs


#### What is Kafka?

Pub-Sub Messaging System.
Producers, Brokers and Consumers.
All data broken into Topics, broken into partitions.

```
    Producer                 [####]

                ,----------------------------------,
                | ,A-,   ,A-,    ,A-,    ,A-,      |
    Broker      | |P0|   |P1|    |P1|    |P0|      |
                | '--'   '--'    '--'    '--'      |
                '----\------\----/-------/---------'
                      '-----,\  /  ,-----'   __________
    Consumer                 [====]         (Zookeeper()
                                            '----------'
```

---

#### Attributes of Kafka Cluster

* Disk Based
> Log oriented system, everything get spooled to disk, retention for any time.
> Like distributed commit log.

* Durable
> Multiple partitions, replicas.

* Scalable
> Just add more partitions to more workers if start maxing out.

* Low Latency
> Add latency when start adding out cluster.

* Finite Retention
> based on time, size or key

* NOT Idempotent (yet)
> can still do idempotent with key-based retention.

---

#### Kafka at LinkedIn

* Multiple Datacenters, multiple clusters in each.
> Breaking things by type of data.

* Mirroring between clusters

* Message Types
> Metrics of all systems
> Tracking, everything users do at frontend
> Queuing, between applications

* Data transport from applications to Hadoop and back.

* Usage statistics
> 300+ Kafka Brokers
> over 18K Topics
> 140K+ partitions
> 220billion msg/day
> 40TB in; 160TB out
> Peak load; 3.25mil msg/sec; 5.5Gb/sec inbound; 18Gb/sec outbound

* each datacenter, one ZK cluster running all Kafka, 5 nodes per cluster

---

#### Challenge Overcome

* Hyper Growth
> a script check data size on disk, make it even, move partitions on the fly

* Logs vs Metrics, logging data kills the metric cluster
> cluster within a cluster; QoS

* Parallel deployment nightmare
> babysit sequential deployments
> Kafka 0.8.1 makes sure the cluster is in good state before shutting down
> any under replicated partitions in brokers, Kafka will not shutdown
> ensures only one broker is in shutdown sequence at a time

* Killing ZK
> every consumer is part of consumer group, every consumer in group can only read each message once
> consumer offset management done within ZK
> every consumer committing offsets every minute for every partition makes ZK sad
> using ZK on SSD

---

#### Monitoring

Most of the consumer problems are with `lag` and read outs.

Lag is difference between where broker is as far as messages, tail the end of log and where is consumer reading it somewhere before.

Consumer lag problems

> * slow application
> * too much GC
> * losing connection to ZK
> * implemented consumer incorrectedly
> * brought up 6 servers in parallel, they hit their 5 sequential rebalances; application panic

Check on Kafka Ecosystem

> * Kafka Brokers
> * ZK
> * Mirror Makers
> * Audit
> * REST Interfaces

##### Cluster Health and Utilization

* Under replicated partitions
> sudden spikes might be load problem, bunch of workers with high under replicated count might be a broker down

* Offline partitions
> number anything other than zero is bad

* Broker partition count
> equal number of partitions

* Data size on disk
> equal amount of data across partitions

* Leader partition count
> only one broker is ever the leader for a partition and communicating
> number of leaders each broker has, automatic feature is not that stable yet

* Network Utilization
> if maxing out, need more workers


##### Zookeeper

* ensemble (cluster) availability

* latency

* outstanding requests


##### Mirror Maker and Audit

* Mirror Maker
> Lag and Dropped Messages

* Audit Consumer
> Lag, Completeness Check

* Audit UI

```
     [Producer]
 msg counts|, |,msg
   [===CLUSTER===]-->{MM}->[====CLUTER====]
     all-msg|, '|audit-suite |     |,  |'
      [Audit Consumer]       |  [Audit Consumer]
                             |
                             |,Audit Suite
                          [Audit UI]
```

---

#### Tuning

##### Kernel Tuning

* swapping is death (but RHEL 6.4 onwards, sawppiness=0 will OM your process so swappiness=1)
* allow more dirty pages
* allow less dirty cache

##### Disk Throughput

* more spindles (RAID10 over 14 disks)
* longer commit intervals

##### JVM

* GC, Garbage First (G1) Collector from Java7 update 51
> set heap size and specify GC pause time, don't set new size... let it figure out

---

#### Coming 0.8.2

* Consumer offsets in broker

* Delete Topic
> don't run the one in 0.8.1, it crashes the cluster

* Further down the road
> * New Producer
> * Improved Producer API

---
---
