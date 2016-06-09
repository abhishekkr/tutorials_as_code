## Scalable and Reliable Logging at Pinterest
> source: DataEngConf 2016, Hakka Labs
> speakers: Krishna Gade, Yu Yang
[youtube:uri](https://youtu.be/DphnpWVYeG8?t=878)

#### Data Pipeline

```
                     _______     ____________
                     |Storm |--->| Stingray |----------,   __
                     '--,,--'    '\________/'           \-|  |
       ,->["]---s-,    _|________                         | PRODUCT
      / backend    \  |[][][][][]|  _________________     |__|
 [Mob]--->["]---s---->|_KAFKA____|  [Pinbal | Skyline]    '|'
 [App]\   app      /    |           '----------------'    /
       '->["]---s-'    .|,           ____'|'___,|.____,--'-->[Pinalytics]
                \    |--------|     |   Qubole        |  \
            *Singer  | Merced |     |_(Hadoop,Spark)__|   \->[A/B Testing]
                     |________|          '|'   ,|.        |
                              \________,[{S3}{S3}{S3}]    '->[Redshift]

```

---

#### Pinterest's Logging Infra

* 1000s of hosts

* '>120 billion' messages, 10s Terabytes per day

* '>500' kafka brokers as central message transportation hub

---

#### Logging Pipeline v1

```
                  KAFKA_0.7_          Real-time consumers
         |Host|   |[][][][][][][] |--->[Apache Storm]
 [Mob]<-->[app]-->|[data uploader]|
 [App]    [app]-->|_______________|-->[{S3}]-->[hadoop]
          [app]-'

```

---

#### Problems with Kafka

##### v0.7

* no replication, data loss on broker failure or high back-pressure

* broker replacement, reboot all dependent services to pick up the latest broker list

##### v0.8, challanges with replication

> topic leader partition can change frequently

* multiple copies among brokers, directly persisting need to know which broker is the leader of topic partition

* can't randomly pick Kafka brokers to write, need leader of each topic partition

* handle corner cases like when a kafka partition is unavailable, or in middle of leader election

---

#### Logging Pipeline v2

```
                  KAFKA_0.8_______     Real-time consumers
         |Host|   |[][][][][][][] |--->[Apache Storm, Spark]
 [Mob]<-->[app]   |[data uploader]|
 [App]    [app]   |_______________|  [Sanitizer]
          .|,     /     .|,          '|' .|,
        [Singer],'   [Secor/Merced]-->[{S3}]-->[hadoop]
```

---

#### `Singer` Logging Agent

Applications log to disk, `Singer` monitors filesystem events and uploads to Kafka.

Isolate applications from Singer agent failures. Isolate applications from Kafka failures.

More than 100MB/second for log files in Thrift.

Production environment support
* dynamic configuration detection
* adjustable log uploading latency
* auditing
* heartbeat mechanism

##### Internals

Staged Event Driven Architecture

```
    |Log.Configuration|
   |================|<------[Configuration.Watcher]
  |===============|                 _____________
        |               _           |Reader-->   |
       .|,         ,-->|_| [] [] -->|_-->Writer__|-,  /''''\
  |'''''''''''''--'     _           _____________   \ |Log |
  | Log.Stream |------>|_| [] [] -->|Reader-->   |   \|Repo|
  | Monitor    |---,                |_-->Writer__|----|    |
  |____________|    \   _           _____________   / \____/
                     ->|_| [] [] -->|Reader-->   |-'
                                    |_--Writer___|
```
---

### Running Kafka in Cloud

#### Challenges

* brokers can die unexpectedly
* disk i/o perf can degrade due to resource contention
* avoid virtual hosts co-location on same physical host
* faster recovery

##### In Practice

* started with c3.2xlarge+EBS
* now d2.xlarge, local disks to avoid EBS contention
* minimize data on each broker for faster recovery
* availability zone aware topic partition allocation
* multiple small clusters (20-40 brokers) for topic isolation

---

#### Scalable Data Persistence (using Secor)

* atomic consistency
* fault tolreance
* load distribution
* horizontal scalability
* configurable upload policies

##### Secor, consensus-based workload distribution

* uses high-level Kafka Consumer distributed over multiple workers
* uses ZK to keep track of each topic from workers to avoid duplication, strong consistency
* fault-tolreant, workers can crash and load is managed

##### Challenges with consensus-based workload distribution

* kafka consumer group re-balancing prevent consumer from making progress in high data volume
* worsen by high-level consumer lags behind same topic partitions
* manual tuning for workload distribution
* inconvenient to add new topics

##### Merced

* central workload distribution
* master-worker topology decoupled with Zookeeper to keep tap on others
* master creates tasks, workers pick up and perform, master keeps track of progress

---
---
