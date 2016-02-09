### Blueflood - the multi-tenated Time-Series Datastore
#### RackSpace

### Compared to Others

* KairosDB
> Put everything into one big con family

* InfluxDB
> Doesn't scale-up to requirements, some use Vertica(Paid Solution)

* OpenTSDB
> Like metric-tagging; don't pre-calculate roll-ups; Uses HBase for name-nodes so single point failure

---

### What's New from last talk on Blueflood, now that's it in Production

* REST API

* ElasticSearch Indexer, to query metric

* Exporters (to Kafka and others)

* Batch Operations

* More StatD Support (as a new datat-type support more than Gauges)

* Ingestion Integrations (getting metrics via REST, StatsD, Logstash, CollectD-WIP)

* Visual Integrations (Graphite-Web, Grafana)

---

### Cloud Metrics (Team that uses Blueflood)

Our Prod Cluster:
* 32 node Cassandra Cluster
* 2 Ingestion Nodes
* 2 Roll-up Nodes
* 3 ES nodes for indexing
* augment space on 32-node cluster with CloudBox storage
* Replication Factor 3
* Consistency Level 1

* 6 billion data-points a day

* Upgraded to Cassandra 2.0

* Moving to OnMetal (not VM but Elastic and SSD)

---

### Architectural Changes

* Gotta move to native DataStax driver

---

* Stats go through Graphite API, so all Graphite features are available.

---
---
