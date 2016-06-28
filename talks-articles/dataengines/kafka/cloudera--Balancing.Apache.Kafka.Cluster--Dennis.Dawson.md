## Balacncing Kafka Clusters
> Cloudera
> by Dennis Dawson

### General Working

* Kafka Cluster get data from producers

* Produces creates messages called topics

* Kafka Cluster comprised of one more server 'brokers'

* Topics are stored in partition

* A broker is assigned as `leader` to handle r/w request for each partition

* Kafka assigns 0+ followers to passively replicate `leader` partition
> replication across configurable number of servers provider fault-tolreance, if broker goes offline
> replicating across multiple brokers preserve data

* consumers of data partition can have steady flow fro cluster, regardless of broker

* producer might produce multiple partitions, each gets unique parition with replicas distributed between broker and cluster

* multiple producers can produce multiple topics, each topic getting its own partition

* consumer are typically organized as group, distributing responsibility of consuming partitions from cluster

---

### Kafka Cluster Balancing

> don't wait till it's too late, 70% capacity enough to re-balance


* 2 common situations for dis-balance of Kafka Cluster

> * Failure ( when one of brokers go offline )
> * Success ( to accomodate more brokers to handle load )


#### in case of 'failure'

when one broker fails, its 'leader partition' go offline
Leader Relection happen
a replica on other broker become 'leader partition'

when a new node come up
Leader Relection happen
and 'leader partition' is re-assinged, Kafka keeps track

* preventing data loss

small chance of data-loss during `leader re-election`

by default only 'leader' acknowledges a `write`
can configure `kafka.request.required.acks = -1`, all replicas will need to ACK
for safety pick a max X


#### in case of 'success'

say existing brokers have grown on partitions, more CPU load and memory consumption
by default adding new broker wouldn't help, new topics gets assigned to new machine
but workload of other machines remain same

they receive equal share of new jobs, this can be addressed with `partition re-assignment`
this tool `kafka-reassing-partition` can't currently automatically study data distribution in Kafka Cluster and move partition in even load distribution
process is unfortunately **manual**

**moving entire topics is easier than moving partitions**, can take hours in some cases

```
## create list of topics to move
$ cat topics-to-move.json
{ "topics": [
    { "topic": "foo1"},
    { "topic": "foo2" }
  ],
  "version":1
}


## then generate command to list distribution of topics and replicas of their current brokers
#### it then gives list of suggestive location for those partitions on new brokers, change this result
#### to suit your plan and save as 'expand-cluster-reassignment.json'
$ bin/kafka-reassing-partitions.sh --zookeeper localhost:2181
  --topics-to-move-json-file topics-to-move.json
  --broker-list "4"
  --generate


{"version":1,
  "partitions": [
    { "topic": "foo1", "partition": 2, "replicas": [1,2]},
    { "topic": "foo1", "partition": 0, "replicas": [3,1]},
    { "topic": "foo2", "partition": 2, "replicas": [1,2]},
    { "topic": "foo2", "partition": 0, "replicas": [3,2]},
    { "topic": "foo1", "partition": 1, "replicas": [2,3]},
    { "topic": "foo2", "partition": 1, "replicas": [2,3]}
  ]}

{"version":1,                                               ## new json
  "partitions": [
    { "topic": "foo1", "partition": 3, "replicas": 4},
    { "topic": "foo1", "partition": 1, "replicas": 4},
    { "topic": "foo2", "partition": 2, "replicas": 4}
  ]}


## to start re-assignment
$ bin/kafka-reassing-partitions.sh --zookeeper localhost:2181
  --reassignment-json-file topics-to-move.json
  --execute

```

---
---
