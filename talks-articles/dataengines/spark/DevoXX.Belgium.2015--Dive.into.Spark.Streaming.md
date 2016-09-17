
> by Gerard Maas
> at DevoXX Belgium 2015

## Dive into Spark Streaming

* delivers a generic framework for parallel computing in a functional paradigm

* fast growing ecosystem around `Spark Core`
> Spark SQL, dataframe API on top of it
> MLLib, ML out-of-the box
> GraphX, big data graph analytics
> Sparks Streaming, reuses spark to re-enable streaming application

---

### RDDs
> Resilient Distributed Dataset
> unit of comutation in Spark
> a distributed collection as opposed to local collection like your daily programmatic Map or Lists
> they are immutable, memory-intensive and `Caching` is controllable

---

### Spark Streaming

> scalable fault-tolerant stream processing system

```
             ,---------------,
 [ Kafka   ]>|               |>=Databases=>,--------------,
 [ Flume   ]>| [ Spark     ] |             |              |
 [ Kinesis ]>| [ Streaming ] |>=HDFS======>|              |
 [ Twitter ]>|               |             | Applications |
 [ Sockets ]>| [ Spark     ] |>=Server====>|              |
 [ HDFS/S3 ]>|               |             |              |
 [ Custom  ]>|               |>=Streams===>|              |
             '---------------'             '--------------'

```

Micro batching is done over datastreams.
Every micro-batch will be an RDD.
On RDDs we can have `Transformations`
Then `Actions` will allow batches to get from `Transformations` to applications.


##### Transformations

> map, flatmap, filter `[% % % %] -> [& & & &]`
> count, reduce, countByValue, reduceByKey `[% % % %] -> n`
> union, join, cogroup `[% %][$ $] -> [% % $ $]`

---

### Deployment Options

* Local

* Standalone Cluster

* Using a Cluster Manager (like mesos)


#### From Streams to MicroBatches

* within a `batch interval`, `receivers` gather `blocks` in parallel with a `block interval`
> bringing in the concept of partitions

* a formula for number of partitions, `receivers * batchInterval / blockInterval`


#### Importace of Caching

```
rdd.cache() // cache RDD before iterating
```


#### The Receiver Model, Receiver-less

Receiver-less model, **Direct Kafka Stream**

> :) Simplified Psrallelism, Efficient, Exactly-once semantics
> :( Less degrees of freedom


#### Spark Streaming v1.5 made Reactive

> Backpressure support

---

references:

[virdata finding available via Wayback machine](https://web.archive.org/web/20160226161501/http://www.virdata.com/tuning-spark/)
[vanwilgenburg.wordpress.com/2015/02/15/spark-tuning-guide/](https://vanwilgenburg.wordpress.com/2015/02/15/spark-tuning-guide/)
[alvincjin.blogspot.in/2015/02/tuning-spark-streaming.html](http://alvincjin.blogspot.in/2015/02/tuning-spark-streaming.html)

---
---
