## Apache Kafka : 0.8.2 and Beyond
> Jay Kreps (@jaykreps), Engineer at LinkedIn
> HakkaLabs

* all data as real-time streams

```
 ,---------------,
 [Graph, Search, ]  ,---,    ,---,  ,_____   ,____
 [OLAP Store, etc]  |DBs|....|DBs|  |Apps|...|Apps|  Real-Time
 '--------------""  ----'    ----'  -----'   -----'
                \     |     /   ____/   ______/
                 '<, ,|____/,  /       /
                    \(_____)--'-------'
 ,------------,      |     |
 | Monitoring |<-----|  K  |     ,-------------------,
 | & Graphs   |      |  A  |<----> Stream Processing ]      Near
 '------------'      |  F  |     '-------------------'    Real-Time
                     |  K  |     ,-------------------,
 ,------------,      |  A  |<---->     Apps          |
 |Log Search  |<-----|_____|     '-------------------'
 '------------'   ,->(_____)<,
                __|          |
               /             |
   ,--------<-'              '->__________
   | Hadoop |<-------------,   [Relational]  offline/
   '---^----'               '->[Warehouse ]  warehouse
       |                       '-^--------'
       '--->[Reporting]<---------'

```

#### where have we been?

* 0.6,0.7 - Scalability, efficiency, persistence
> okay for log data, okay for stream processing

* 0.8.0,0.8.1 - fault-tolerance, log-compaction, better partitioning
> good for db-data (don't wanna lose), changelogs, better for stream processing

#### what's coming in 0.8.2?

* operational improvements
> topic deletion
> leader balancing
> connection quotas
> latency improvements
> parallel recovery
> better durability and consistency controls

* offset storage
> old, ZK offset storage (few 1000s offset commits per second, no ability to scale)
> so now, New Kafka offset storage (horizontally scalable)
> client de-ZK-fication, phase 2
> uses log compaction internally

* new java producer
> pure java, no scala, minimal dependencies in client jar
> Async vs Sync producer distinction disappears
> always returns the offset or error
> predictable, bounded memory usage
> protocol compatible, but new API
> both producers will exist for some time ( now there is only Async producer )


#### Log Compaction?

* uses
> * DB change capture
> * stateful stream processing
> * event sourcing

```
   offset [ 0 ][ 1 ][ 2 ][ 3 ][ 4 ][ 5 ][ 6 ][ 7 ][ 8 ][ 9 ][ 10]
   key    [ K1][ K2][ K1][ K1][ K3][ K2][ K4][ K5][ K5][ K2][ K6] log before
   value  [ V1][ V2][ V3][ V4][ V5][ V6][ V7][ V8][ V9][V10][V11] compaction
                     | | | | | | | | | |
                     c o m p a c t i o n
                     |,|,|,|,|,|,|,|,|,|,
               [ 3 ][ 4 ][ 6 ][ 8 ][ 9 ][ 10]   log after
         key   [ K1][ K3][ K4][ K5][ K2][ K6]   compaction
         value [ V4][ V5][ V7][ V9][V10][V11]   compaction
```


#### new producer performance

* non-blocking I/O

* better latency/throughput tradeoff

* performance
> * 3x async replication - 786,980 records/sec
> * 3x sync replication  - 421,823 records/sec
> * end-to-end latency   - 1.5ms

```
        old (async) producer


      Sender Process
 ,-------------------------, sequential  __________
 |        Async Producer   | blocking    |         |
 |        ,----------------| i/o         |    :    |
 |User   send()    ,_______|             |    :    |
 |Threads-+-> ____ | I/O   |------------>|[KafkaNode]
 |        +->(___()|Thread |------------>|[KafkaNode]
 |        +-> in-  '-------|------------>|[KafkaNode]
 |        |   mem          |             |    :    |
 |        |   Queue        |             |    :    |
 |        '----------------|             |_________|
 '-------------------------'             Kafka Cluster
```

```
        new producer


      Sender Process
 ,-------------------------, parallel    __________
 |        New   Producer   | pipelined   |         |
 |        ,----------------| i/o         |    :    |
 |User   send()    ,_______|             |    :    |
 |Threads-+->b====b| I/O   |------------>|[KafkaNode]
 |        +->b====b|Thread |------------>|[KafkaNode]
 |        +->b====b'-------|------------>|[KafkaNode]
 |        |'>b====b        |             |    :    |
 |        |Binary Queues   |             |    :    |
 |        '----------------|             |_________|
 '-------------------------'             Kafka Cluster
```

---

### what's next

* new consumer
* security
* more operational improvements
* transactions and idempotence

#### 0.9 : New Consumer (planned)

* counterpart to new producer
* partition assignment moves to the broker
* collapse distinction between 'high-level' and 'simple' consumer
* allow arbitrary control of the offset
* notification when partitions are re-assigned


#### 0.9 : Security (planned)

* authentication (Kerberos, TLS/SSL)
* authorization (pluggable)


#### 0.9 : operational improvements (planned)

* automated partition balancing
* partition movement throttling
* scaling the number of partitions
* quotas/throttling


#### 1.0?? : transactions and idempotence (planned)

* idempotence
> exactly once delivery, retries safe

* transactions
> atomic writes across partitions

---
---
