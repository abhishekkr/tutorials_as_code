
> incomplete book notes

## Kafka - The Definitive Guide

### Intro

* `pub-sub` messaging system coined as `distributed commit log`

> data within Kafka is durable, retention for a time period
> read deterministically
> distributed within the system for protection against failures and utilize scaling opportunity


* unit of data is `message`

> message can have optional bit of metadata referred to as a key
> Keys used to write message to partition in a more controlled manner
> written in batches (produced for same topic and partition), tradeoff between latency and throughput


* schemas

> recommended to impose additional structure, many options as JSON and XML
> Apache Avro serialization, compact and separate from message payloads


* `message`categorized into `topic`; `topics` broken into `partitions`

> partitions ~= hash-ring, assure messages with same key always written to same partition
> partition is a single log
> messages written in append-only, read in chronological order
> no guarantee of time-ordering across partitions for topic, just in a partition
> partitions are the way Kafka provides redundancy and scalability


* `producers` publish; `consumers` subscribe

> producers can direct message to a specific partition using key; can use custom partitioner
> each message in a given partition has a unique offset
> consumers keep track of what has been already read by `offset` of meesages
> marking offset in Zookeeper or Kafka itself, consumer can pause and resume without losing its place
> in `consumer group` each partition is only consumed by one member


* `broker` is single Kafka server, generally good enough for thousand partitions

> brokers designed to operate as part of `cluster`, where one will act as `controller`
> controller assigns partitions to brokers and monitor failures
> partition assigned to multiple brokers will cause `replication`, one acts as `leader` and handles communication for it
> brokers configured with default retention setting for topics by size or time duration (like logrotate)


* `multiple clusters` can be useful for data segregation/isolation or multiple datacenter disaster recovery

> with multiple datacenter,, messages be copied between them
> replication work within a single cluster
> tool `Mirror Maker` helps, its simply a Kafka consumer and producer linked with a queue


* ZK involvement

```
 [producer]---->[Kafka Broker]----->[consumer (old)]
                      |                     |
              Broker and Topic         Customer Metadata
                 Metadata            Partition Offsets
                      |,                    |
                    ,----,                  |
                   (_____)                  |
                   |  ZK |<-----------------'
                   |_____|
```


* one of the Mirror Maker architectures

```
 ,--datacenter-A---------,     ,--datacenter-A---------,     ,--datacenter-A---------,
 | [producer] [producer] |     | [producer] [producer] |     | [producer] [producer] |
 |      |,        '|     |     |      |,        '|     |     |      |'        '|     |
 | [Kafka Cluster Local] |     | [Kafka Cluster Local] |     |      |          |     |
 |           |      \    |     |   /       |           |     | [  Kafka   Cluster  ] |
 |       consume   ,-'------------'    consume         |     | [     Aggregate     ] |
 |           |,  ,/      |     |  \,--,    |,          |     |           |'          |
 |     (MirrorMaker)     |     |     (MirrorMaker)     |     |           |           |
 |           |           |     |           |           |     |        produce        |
 |         produce       |     |         produce       |     |           |'          |``
 |           |,          |     |           |,          |     |           |           |
 | [Kafka Cluster Aggr.] |     | [Kafka Cluster Aggr.]--consume-->(Mirror Maker)     |
 |                       |     |                       |     |                       |
 '-----------------------'     '-----------------------'     '-----------------------'
```


* seamlessly handles multiple producers to multiple consumers

> aggregating data from multiple front giving unified view as a single topic
> consumers don't interfere with streams, can act alone or part of group

---

### Zookeeper Setup

* this flow involves JDK v8 u51, ZK 3.4.6

* Standalone Server

> following installs ZK with basic config at `/usr/local/zookeeper`
> storing data at `/var/lib/zookeeper`

```
export JAVA_HOME=/usr/java/jdk1.8.0_51
ZK_VERSION="3.4.6"
ZK_DIR="/usr/local/zookeeper"
ZK_DATADIR="/var/local/zookeeper"
ZK_CLIENT_PORT=2181

tar -zxf "zookeeper-${ZK_VERSION}.tar.gz"
mv "zookeeper-${ZK_VERSION}" $ZK_DIR
mkdir -p $ZK_DATADIR

cat > "$ZK_DIR/conf/zoo.cfg" <<EOF
tickTime=2000
dataDir=$ZK_DATADIR
clientPort=$ZK_CLIENT_PORT
EOF

$ZK_DIR/bin/zkServer.sh start
```

* to validate ZK running, connect to port and send 4 letter 'srvr'

```
telnet localhost 2181
...
srvr
.......this shows metadat from server
```

#### Zookeeper `ensemble` 

* it's name for cluster


* need odd number servers for Quorum, due to consensus protocol used, need majority number running

> means 3-node servers can run with one missing, 5-node ensemble with 2 missing


* not recommended to run ensemble larger than 7 nodes, performance can degrade


* Configuring Zookeeper in ensemble must have common configuration

> each server needs `myid` file in data directory, specifying id number of servers

```zoo.cfg
tickTime=2000
dataDir=/var/lib/zookeeper
clientPort=2181
initLimit=20
syncLimit=5
server.1=zoo1.myensemble.internal:2888:3888
server.2=zoo2.myensemble.internal:2888:3888
server.3=zoo3.myensemble.internal:2888:3888
```

* here `initLimit` is amount of time to allow followers to connect to leader


* `syncLimit` limits how far out of sync followers can be with leader


* `initLimit` and `syncLimit` are unit of `tickTime`, which is value in millisecond


* server specified format `server.X=hostname:peerPort:leaderPort` got

> X as ID number of server, must be unique integer, needn't be zero-based or sequential
> hostname is hostname or IP address of server
> peerPort is TCP port over which servers in ensemble communicate with each other
> leaderPort is TCP port over which leader election gets done


* clients only need to connect over clientPort, ZK nodes need to reach each other at all three

---

### Setup Kafka Broker

> at print Kafka version 0.9.0.1 running Scala version 2.11.0

```
SCALA_VERSION="2.11.0"
KAFKA_VERSION="0.9.0.1"
KAFKA_DIR=/usr/local/kafka

tar -zxf "kafka_${SCALA_VERSION}-${KAFKA_VERSION}.tar.gz"
mv "kafka_${SCALA_VERSION}-${KAFKA_VERSION}" $KAFKA_DIR
mkdir /tmp/kafka-logs

$KAFKA_DIR/bin/kafka-server-start.sh -daemon $KAFKA_DIR/config/server.properties
```


* verifying Kafka broker is active

```
## create topic
$KAFKA_DIR/bin/kafka-topics.sh --create --zookeeper localhost:2181 \
    --replication-factor 1 \
    --partitions 1 \
    --topic test

## verify topic
$KAFKA_DIR/bin/kafka-topics.sh --zookeeper localhost:2181 \
    --describe \
    --topic test

## produce messages to a test topic
$KAFKA_DIR/bin/kafka-console-producer.sh --broker-list localhost:9092
    --topic test
Some Message
Another Message
^D

## consume messages
$KAFKA_DIR/bin/kafka-console-consumer.sh --zookeeper localhost:2181
    --topic test \
    --from-beginning
Some Message
Another Message
^C
```


#### Broker Configuration

* `broker.id`

> Every broker must have integer identifier. Default set to `0`.
> Selection is arbitrary, can be moved among brokers for maintenance tasks

* `port`, default listener on 9092 

* `zookeeper.connect`, location for ZK storing broker metadata

> with format `hostname:port/path`, '/path' is optional to use **chroot** environment for Kafka cluster
> chroot path will get created if doesn't exists, good practice as allows ZK to be shared
> multiple ZK servers of same ensemble can be specifed with semicolon separation

* `log.dirs`, persists all messages to disks in log segments under this directory

> multiple paths can be provided with comma separation
> 'least picked' segment will get picked for a partition, one partition will go to one location only

* `num.recovery.threads.per.data.dir`, default only 1 thread per log dir is used

> used only at start or shutdown, so reasonable for high number to parallelize tasks

* `auto.create.topics.enable`, auto creates a topic on pub/sub to/from a topic or requesting its metadata

#### Topic Defaults

> Old version provided broker configs to specify per-topic override using `log.retention.{hours,bytes,segment}.per.topic`.
> Now overrides must be specified using administrative tools.


* `num.partitions`, determines how many partition a new topic creates

> defaults to 1, can only be increased once created
> the way topic is **scaled** within cluster when it has high message volume


* `log.retention.ms`, how long Kafka will retain messages by time

> default specified by `log.retention.hours` to 168hours or one week
> this **faults** when moving partition between brokers, as this depends on timestamp


* `log.retention.bytes`, retention based on total bumber of bytes of messages

> applied per-partition, all retention is performed for an individual partition not topic


* `log.segment.bytes`, control closing log segments when they reach a size

> only closed segments be expired, so volume of messages are important to mark this
> say if limit takes 7 days to reach and has a week retention to expire, a segment will then persist 14 days

> > Size of log segments also affects behavior of fetching offsets by timestamp.
> > Kafka looks for log-segment in partition with last-modified of file is (thus closed) after the timestamp and immediately previous segment was last  modified before timestamp.
> > Kafka returns offset at beginning of that log-segment (also the filename).
> > Smaller log segments will provide more accurate answers for offset requests by timestamp.


* `log.segment.ms`

> important to consider disk cost when multiple segments are closed simultaneously


* `message.max.bytes`, limits producer on size of message, default 1MB

> noticeable performance impacts on increasing size, increased network and disk I/O throughput
> can be coordinated with `fetch.message.max.bytes`, `replica.fetch.max.bytes` to error at consumer and replica nodes

---

### Hardware Selection

* Disk Throughput

> Broker disk directly influences producer client performance.
> Most clients wait until at least a broker confirms.
> SSDs got drastically low seek-access time, provide best performance.
> Can optimize spinning drives by using more of them with multiple data directories, or setting in RAID.


* Disk Capacity

> Driven by how many messages, in how many segments, for how long. Also replication.


* Memory

> It mostly affects consumers. Kafka itself doesn't need much heap.
> Broker handling X messages/sec and data-rate X MBs/sec can run with 5GB heap.
> Need to use resr of memory as Page Cache.


* Networking

> Complex combination of inbound/outbound network usage based on producers and consumers.


* CPU

> Kafka broker decompress every message batch in order to assign offsets.


#### Kafka in Cloud

For low latency, instance with optimized I/O and local SSD.


#### Kafka Clusters

* all brokers need same `zookeeper.connect` config and unique integer `broker.id`


#### OS Tuning

* Virtual Memory - Swappiness

> mostly sysctl tweaks to follow

> generally they auto-adjust, for high throughput should avoid swapping
> Kafka use page cache heavily, VM swapping to disk incurs heavy cost, not enough memory allocated to page cache
> configuring `vm.swappiness` to `1` makes likeliness of swap to bare minimum, it zero disables on some


* Virtual Memory - Dirty Pages

> Log segments put on SSD or subsystem with significant NVRAM for caching (eg RAID),
> to reduce count of dirty pages allowed before flush bg-process starts writing to disk
> set `=vm.dirty_background_ratio+` value lower than default(10)
> its % of total system memory; configuring it 5 is appropriate for most cases
> these can also be increased changing `vm.dirty_ratio` above default(20), also % of sys-mem
> increasing this demands high level of replication to be used for data safety
> current count of dirty pages can be checked under `/proc/vmstat`

```
cat /proc/vmstat | grep -E 'dirty|writeback'
nr_dirty 3
nr_writeback 0
nr_writeback_temp 0
nr_dirty_threshold 173829
nr_dirty_background_threshold 86914
```


* Disk

> other than hardware and config of RAID, filesystem has big impact
> most common choices are EXT4 or XFS
> XFS can perform better with less tuning
> EXT4 can outperform but with less safe considered configs, like increasing commit interval to force less frequent flush
> regardless of filesystem, set `noatime` mount option for log-segment mount point
> `atime` is last access timestamp, changed for every read, generates large disk writes; not used by Kafka 


* Networking

> * first is to change default and max memory amount for send and receive buffers for each socket to increase performance for large transfers
> relevant parameters for send and receive default buffers per socker are `net.core.wmem_default` and `net.core.rmem_default`, reasonable seeting for these is 131072 or 128KBs
> param for maximum buffer are `net.core.wmem_max` and `net.core.rmem_max` with 2097152 (2MB) as reasonable config
>
> * in addition, buffer for TCP sockets must be set separately using `net.ipv4.tcp_wmem` and `net.ipv4.tcp_rmem` configured with 3 space separated integeres for minimum, default and maximum
> maximum shan't be more than `net.core.wmem_max` and `net.core.rmem_max`
> example value is `4096 65536 2048000`
>
> many more tweaks,
> * enabling TCP window saling `net.ipv4.tcp_window_scaling` to 1, allow buffer on broker side
> * increase value on `net.ipv4.tcp_max_syn_backlog` above default(1024) will allow greater simultaneous connection
> * increase `net.core.netdev_max_backlog` above default(1000), assists with bursts of network traffic by allowing more packets to be queued up for kernel to process


#### Production Concerns

* GC Options

> G1 is designed to adjust to different workloads and provide consistent pause times for GC over app lifetime. 2 config options often used for it
> * MaxGCPauseMillis specifying preferred pause time for each GC cycle, defaults 200ms, its not strict limit and exceeded if required
> * InitiatingHeapOccupancyPercent specify % of total heap to fill up before cycle starts, default 45%, incl. eden and old zone usage

Start script of Kafka doesn't use G1 as of now, instead defaulting to Parallel New and Concurrent Mark ans Sweep GC.
Change is easy

```
export JAVA_HOME=/usr/java/jdk1.8.0_51
export KAFKA_JVM_PERFORMANCE_OPTS="-server -XX:+UseG1GC \
                                   -XX:MaxGCPauseMillis=20 \
                                   -XX:InitiatingHeapOccupancyPercent=35 \
                                   -XX:+DisableExplicitGC -Djava.awt.headless=true"
$KAFKA_DIR/bin/kafka-server-start.sh -daemon $KAFKA_DIR/config/server.properties
```


* Kafka brokers got no rack or zone awareness for assigning partitions.


* Colocating applications on ZK

> Prior to Kafka 0.9.0.0, consumers utilized ZK to directly store composition of consumer group, topics consumed and offset.

There is a **concern** with consumers and ZK under certain configs. Consumers have configurable choice to use ZKor Kafka for committing offsets and interval between commits.
When ZK is used, each consumer will perform a write every partition consumption.
Recommended latest Kafka libraries use Kafka for committing offsets, removing ZK dependency.

Other than using ZK ensemble for multiple Kafka Clusters, not recommended to share with other applications. Kafka is sensitive to ZK latency/timeouts. It's the lifeline.

---

### Kafka Producer - Writing Messages to Kafka

Alongwith built-in clients, Kafka has a binary wire protocol. Multiple languages have libraries implementing it.
[Apache Kafka Clients](https://cwiki.apache.org/confluence/display/KAFKA/Clients)


#### Producer Overview

main steps involved in sending data to Kafka

```
 ,----------------,          ,--------------------------------------------------,
 | ProducerRecord |          |                     Producer       ,-----------, |
 | ,-------------,| send()   |                                    |Topic.A    | |
 | [    Topic    ]|--------, |                                    |Partition.0|---,
 | [ {Partition} ]|        | |                                  ,-| [batch.0] | | |
 | [    { Key }  ]|        '----->[Serializer]-->[Partitioner]--| | [batch.1] | | |    ,-'''''-,
 | [   Value     ]|          |                                  | | [batch.2] | | |--->| Kafka |
 | '-------------'|          |                                  | '-----------' | |    |Broker |
 '----------------'          |                                  |               | |    '-,,,,,-'
          |'                 |                                  '-,-----------,---'        |
          |                  |                                    |Topic.B    | |          |
          |                  |                                    |Partition.1| |          |
          |                  |                                    |           | |          |
          |                  |                                    | [batch.0] | |          |
          |                  |                                    | [batch.1] | |          |
          |                  |                                    | [batch.2] | |          |
          |                  |                                    '-----------' |          |
          |                  |                                   yes|'          |          |
          |                  |                                      |           |          |
          | if can't retry, throw exception                  ,------'           |          |
          |------------------|----------------------------{Retry?}<----{Fail?}<------------'
          |                  |                                      yes  |      |
          |                  |                                           |      |
          |                  '-------------------------------------------|------'
          | when successful, return metadata                             |
          '--------------------------------------------------------------'
```

* ProducerRecord must include `topic` to target and `value` to send. Optionally can specify a key and/or partition.

> By default `partitioner` will chose a `partition`, usually based on ProducerRecord key.


* Producer first serializes key and value objects to ByteArrays to be sent over network.

> Once producer knows `topic` and `partition` for a record, sends batch of records to same. In a separate thread.


* Broker sends back a response if message received successfully.

> Returns `RecordMetadata` object with `topic`, `partition` and `offset`.
> Returns error on failure.


#### Constructing a Kafka Broker

First step is creating a Producer object with properties you wanna pass to producer, 3 mandatory properties

* 1. `bootstrap.server`; list of 'host:port' pairs of Kafka brokers

> doesn't need to be complete, more like should be able to help Producer query brokers which can help query more and let it have complete set


* 2. `key.serializer`; brokers expect ByteArrays of key-val although Producer interface allows sending any Java object as key-val

> so Producer need name of class implementing required serializer as interface to `org.apache.kafka.common.serialization.Serializer`
> Kafka client got few built-in serializers like ByteArraySerializer, StringSerializer, IntegerSerializer
> required if you intend to send only values


* 3. `value.serializer`; similar to Key-object by `key.serializer` this is required to serialize Value object

> both needn't be same thus different attributes, like integer keys for string values


Snippet on how to create a new Producer

```java
private String bootstrapServers = "broker01:9092,broker02:9092";
private String keySerializer = "org.apache.kafka.common.serialization.StringSerializer";
private String valueSerializer = "org.apache.kafka.common.serialization.StringSerializer";

private Properties kafkaProps = new Properties(); // Properties object for providing to Producer

kafkaProps.put("bootstrap.servers", bootstrapServers);
kafkaProps.put("key.serializer", keySerializer);
kafkaProps.put("value.serializer", valueSerializer);
// default properties gets used for everything else unless provided

producer = new KafkaProducer<String, String>(kafkaProps); // creating new Producer
```

[correct config options for Producer are must](http://kafka.apache.org/documentation.html#producerconfigs)


#### Sending messages to Kafka

First, need to create a ProducerRecord that could be sent

```
ProducerRecord<String, String> record =
    new ProducerRecord<>("HTTPRequests", "2016-08-21 17:54:55", "all-http-logs-for-this-time");
```


**There are 3 primary methods**

* `fire-and-forget`

```
try {
    producer.send(record);
} catch(Exception e) {
    e.printStackTrace();
}
```

* `synchronous-send`, use `send()` method to get a Future object and use `get()` to wait on it

```
producer.send(record).get();
```


* `asynchronous-send`, use `send()` method with a callback function

```
private class DemoProducerCallback implements Callback {
    @Override
      public void onCompletion(RecordMetadata recordMetadata, Exception e) {
        if (e != null){
            e.printStackTrace();
        }
      }
}

// ---

producer.send(record, new DemoProducerCallback());

```


#### Serializers

> Producer configuration includes mandatory serializers.


* Custom Serializers

> To serialize more generic records, write custom serializer...
> introduce Avro (or Thrift or Protobuf) serializer as a recommended alternative.
> To create records, or create custom serializer for objects.

[Customer.java](./Customer.java)
[CustomerSerializer.java](./CustomerSerializer.java)

Debugging compatibility issues between different versions of Serializer and Deserializer, challenging between multipe teams.
So, better not to use a custom serializer but an existing protocol as Apache Avro, Thrift or Protobuf.


* Serializing using Apache Avro




* Using Avro records with Kafka


#### Partitions



#### Configuring Producers



#### Old Producer APIs



---




---
---
