
## Reindexing 36 billion documents in 5 days within same ElasticSearch cluster

[source](https://thoughts.t37.net/how-we-reindexed-36-billions-documents-in-5-days-within-the-same-elasticsearch-cluster-cd9c054d1db8)

* Updating ES mapping on a large index is easy until existing type field is change or deleted. It requires complete reindexing in a separate index created with right mapping.


### The `Blackhole` cluster

* thought of a hot datastore (slow to read) with `14*2` core CPU, `512GB` mem and `36 * 800GB` SSD, each running multiple ES instances

* picked (horizontally scalable) 75 physical machines; with 2 http nodes behind HAProxy per DC, 3 master nodes in 3 DCs, 70 data nodes int 2 DCs

> each node with `quad core Xeon CPU 2.40GHz` and `64GB` memory; RAID-0 over `4*800GB` SSD with XFS


### ES config

* runs ES 1.7.5 on Java 1.8; indexes with 12 shards and 1 replica

> ensured each DC hosts 100% data using ES [rack awareness](https://www.elastic.co/guide/en/elasticsearch/reference/current/allocation-awareness.html)

* all filtered queries ran with `_cache=false`, to avoid search caching to explode cluster

* production ES config as below

```
routing:
  allocation:
    node_initial_primaries_recoveries: 20
    node_concurrent_recoveries: 20
    cluster_concurrent_rebalance: 20
    disk:
      threshold_enabled: true
      watermark:
        low: 60%
        high: 78%
index:
  number_of_shards: 12
  number_of_replicas: 1
  merge:
    scheduler:
      max_thread_count: 8
      type: 'concurrent'
      policy:
        type: 'tiered'
        max_merged_segment: 100gb
        segments_per_tier: 4
        max_merge_at_once: 4
        max_merge_at_once_explicit: 4
  store:
    type: niofs
  query:
    bool:
      max_clause_count: 10000

action:
  auto_create_index: false

  indices:
    recovery:
      max_bytes_per_sec: 2048mb
    fielddata:
      breaker:
        limit: 80%
      cache:
        size: 25%
        expire: 1m
    store:
      throttle:
        type: 'none'

discovery:
  zen:
    minimum_master_nodes: 2
    ping:
      multicast:
        enabled: false
      unicast:
        hosts: ["master01","master02","master03"]

threadpool:
  bulk:
    queue_size: 3000
    type: cached
  index:
    queue_size: 3000
    type: cached
bootstrap:
  mlockall: true
memory:
  index_buffer_size: 10%
http:
  max_content_length: 1024mb
```

* use [niofs](https://www.elastic.co/guide/en/elasticsearch/reference/current/index-modules-store.html) for ES filesystem storage, allows concurrent read from same file for shard index


### Tuning JVM

* launched with `-Xms31g -Xmx31g`, combined with `mlockall=true` ensuring ES gets enough memory to run and never swaps; remaining `33GB` used for ES threads and FS cache

* despite ES recommendations, replace [Concurrent Mark Sweep](https://docs.oracle.com/javase/8/docs/technotes/guides/vm/gctuning/cms.html) GC with [Garbage First Garbage Collector](http://www.oracle.com/technetwork/tutorials/tutorials-1876574.html); with simple `G1GC` config to do job under pressure

```
JAVA_OPTS=”$JAVA_OPTS -XX:-UseParNewGC”
JAVA_OPTS=”$JAVA_OPTS -XX:-UseConcMarkSweepGC”
JAVA_OPTS=”$JAVA_OPTS -XX:+UseCondCardMark”
JAVA_OPTS=”$JAVA_OPTS -XX:MaxGCPauseMillis=200"
JAVA_OPTS=”$JAVA_OPTS -XX:+UseG1GC “
JAVA_OPTS=”$JAVA_OPTS -XX:GCPauseIntervalMillis=1000"
JAVA_OPTS=”$JAVA_OPTS -XX:InitiatingHeapOccupancyPercent=35"
```

> with CMS it would run into stop the world GC on query over more than one month data


### Blackhole Initial Indexing

* took 19 days fetching and pushing initial data

* decided with 1 index per month and 30 shards per index; didn't work as each query on a month requests data from 3TB and 1.2 billion documents

* merge and indexing took part on 8 VMs, each running 8 indexing processes

> indexer was shard aware; had mapping between index to write on, shards and data node
>
> allowed to index directly on right data nodes with lowest latency

* indexer were slowed for not able to read more than 10K documents per second per Kafka partition; also CPU was bigger bottleneck than disk i/o


### Blackhole re-indexing

* split up cluster into daily indexes; going daily reduced average index from 3TB to 120GB

* ran re-indexers on Blackhole data nodes itself

* used Logstash to transform data model as it has everything required, with following config

```
input {
  elasticsearch {
    hosts => [ "local elasticsearch node" ]
    index => "index to read from"
    size => 5000
    scroll => "20m" # 5 minutes initial
    docinfo => true
    query => '{ "query": { "range": { "date": { "gte": "2015-07-23T10:00.000+01:00", "lte": "2015-07-23T11:00.000+01:00" } } } }'
  }
}

output {
  elasticsearch {
    host => "remote elasticsearch node"
    index => "index to write to"
    protocol => "http"
    index_type => "%{[@metadata][_type]}"
    document_id => "%{[@metadata][_id]}"
    workers => 10
  }
  stdout {
    codec => rubydebug # because removing the timestamp field makes logstash crash
  }
}

filter {
  mutate {
    rename => { "some field" => "some other field" }
    rename => { "another field" => "somewhere else" }
    remove_field => [ "something", "something else", "another field", "some field", "@timestamp", "@version" ]
  }
}
```

* few configs changed for reindexing at ES

```
memory:
  index_buffer_size: 50% (instead of 10%)
index:
  store:
    throttle:
      type : "none" (as fast as your SSD can go)
  translog:
    disable_flush: true
  refresh_interval: -1 (instead of 1s)
indices:
  store:
    throttle:
      max_bytes_per_sec: "2gb"
```

* built tools to keep a record of indexes to process in separate DB and orchestrate with re-try:w

---

### Migrating BlackHole to ES6

[updates from new article](https://thoughts.t37.net/migrating-a-130tb-cluster-from-elasticsearch-2-to-5-in-20-hours-with-0-downtime-and-a-rollback-39b4b4f29119)

* Reindex API has suboptimal errorhandling so can lose documents, also is slow

* Logstash has race condition problem, also has chance of loosing data in process

> * throw more hardware at Blackhole as migration would put more load
>
> * to make sure cluster to use as much disk as possible, raised watermark thresholds to max

```
"cluster.routing.allocation.disk.watermark.low" : "98%" "cluster.routing.allocation.disk.watermark.high" : "99%"
"indices.recovery.max_bytes_per_sec": "4096mb"
"indices.recovery.concurrent_streams": "50" 
 
"cluster.routing.rebalance.enable": "none"
```

* transferring TBs of data puts huge load and fills ES [bulk thread pool](https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-threadpool.html) with risk of rejected data

* ES provides `zone`, combined with `rack awareness` for better allocation granularity

> created a zone holding only daily data for less load of migration, disabled recovery and forced indixes allocation

```
PUT /_cluster/settings {
  "transient" : {
    "cluster.routing.allocation.enable" : "none"
  }
}

PUT /*/_settings {
  "index.routing.allocation.exclude.zone" : "fresh"
}

PUT /latest/_settings -d {
 "index.routing.allocation.exclude.zone" : "",
 "index.routing.allocation.include.zone" : "fresh"
}
```

> when cluster got quiet, resumed migration

```
PUT /_cluster/settings {
  "transient" : {
    "cluster.routing.allocation.enable" : "all"
  }
}
```

---
