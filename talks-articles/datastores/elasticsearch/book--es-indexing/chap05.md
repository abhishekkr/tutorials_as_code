
## 05. Anatomy of an Elasticsearch Cluster

### Basic Concepts

* when a node starts, ES creates a directory based on cluster name and node is allocated to this directory

* in background, ES creates some shards & replicas probably (unless explicitly configured not to); allocates shards in same node

* increasing node, cluster reorganizes itself to distribute load


### Node

* single instance of ES server, can host data by default by getting allocated for shards of indices

#### Non-Data Nodes

* `node.data: false` in `elasticsearch.yml` to run a non-data node

* 2 types
> * dedicated master nodes: with `node.master: true`, such nodes manage cluster; don't cater to index or search requests
>
> * client nodes: with `node.master: false` used to balance the load for HTTP communication


#### Remote-Eligible Node

* `node.roles: [remote_cluster_client]` have capability to act as client for remote clusters

> earlier there used to be Tribe Node (DEPRECATED); to access data among multiple clusters, it acts as a federated client across clusters
>
> for current cross-cluster config, [check this](https://www.elastic.co/guide/en/elasticsearch/reference/7.x/modules-remote-clusters.html)

* Remote Clusters can be added using `Sniff Mode` (seed list) or `Proxy Mode` (single proxy socket to be setup)

* Search & Replication is available to Remote Clusters

### 7.x Types

* dedicated master eligible node: `node.roles: [master]`; is eligible if not a `voting_only` in roles list which is needed for HA cluster that need minimum 3 master eligible nodes but can do away with 2 dedicated master nodes

* data nodes of variety `node.roles: [data]` to have separation of data and master nodes

> in a multi-tiered deployment, can have further categories
>
> * `data_content` to accomodate user-created content
>
> * `data_hot` store time-series as it enters ES, fast read/writes
>
> * `data_warm`, low frequency query volume
>
> * `data_cold`, storing read-only indices
>
> * `data_frozen`, storing searchable snapshots

* `node.roles: [ingest]` for pre-processing pipelines, composed of one or more ingest processors

* `node.roles: []` are coordinating only node, behave as smart load-balancers; adding too-many of these will slow down Masters as well for waiting on their states

* `ml` for machine-learning nodes


### Shards

* creating an ES index, ES subdivides into multiple Lucene indices called Shards

* Shards are in themselves functional & independent index; allowing horizontal scaling search for a content volume


### Replicas

* by default ES creates copy of Primary Shards; used to improve search performance & failover

* replicas can be added/removed later stage as well; easily


### Explaining the Architecture of Distribution

* `index.number_of_shards` defaults to 1, can only be set at index creation and max of 1024 per index

* can set custom attributes like `node.attr.rack_id` for shard allocation awareness; and enabling `cluster.routing.allocation.awareness.attributes: rack_id ` in master-eligible nodes

* get created shards and their state by `curl -X GET "localhost:9200/_cat/shards?pretty"`; on a single node set-up it will show default 1 shard (STARTED) per index and 1 replica (UNASSIGNED)

* thus cluster health API `curl -X GET "localhost:9200/_cat/health?pretty"` will give `yellow` state and `50%` healthy


### Correctly configuring the cluster

* when the health status is `red`, means one of primary shards is not active; without replicas losing data is inevitable

* each shard has an overhead; in usual cases small set of large shards run low overhead than many more smallshards

* searches run on a single thread per shard

* delete indices not documents wherever possible, ES marks the document deleted and only frees on a periodic segment merge

* use data-streams and ILMs for time-series data

* aim for shard-sizes between 10GB to 65GB

```
curl -X GET "localhost:9200/_cat/shards?v=true&h=index,prirep,shard,store&s=prirep,store&bytes=gb&pretty"
```

* aim for less-than-20 shards per GB of heap memory

```
curl -X GET "localhost:9200/_cat/nodes?v=true&h=heap.current&pretty"
```

* explicitly limit max shards on a node, to avoid hotspots

* delete empty/unneded indices; force merge during off-peak hours; shrink an existing non-writable index to fewer shards

* can use `reindex` API, to comobine similar smaller indices to a single large index

* [split](https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-split-index.html) and existing index into more, for more primary shards

* use [rolling restarts](https://www.elastic.co/guide/en/elasticsearch/guide/current/_rolling_restarts.html) to not loose data during maintenance


### Choosing the right amount of Shards and Replicas

> (max number of data-nodes) = (number of shards) * (number of replicas + 1)

---
