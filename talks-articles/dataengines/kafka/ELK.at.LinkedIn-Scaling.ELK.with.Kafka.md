
## Scaling ELK with Kafka
**ELK @ LinkedIn**
> Tin Le, SRE @ LinkedIn
> LinkedIn Talks at Yotube


#### ELK at LinkedIn

100+ ELK clusters across 20+ teams and 6 DataCenters.

Some of large clusters have
* `>32+` billion docs (30+ TB)
* daily indices 3billion docs (~3TB)

---

#### Logging Using Kafka at LinkedIn

* dedicated Kafka clusters for log in each DataCenter, no replication among DCs as yet

* individual topics per application (currently 30K topics)

* defaults to 4 days of transport level retention

* common logging transport for all services, languages and frameworks

---

#### ELK Architectural Concerns

* Network Concerns

> Bandwidth, Network partitioning, Latency


* Security Concerns

> Firewalls and ACLs, Encrypting data in transit


* Resource Concerns

> misbehaving application can swamp production resources

---

#### ELK Search Architecture

```

   [ElasticSearch]<----------------------------->[ElasticSearch]
   [===master====]                               [=====tribe===]
    /'______    '\_________                             '|,
 |[ESearch ]|   |[ESearch ]|                     [===Kibana====]
 |[datanode]|<->|[datanode]|                            '|,
 |    |'    |   |    |'    |                          {users}
 |[Logstash]|   |[Logstash]|
 '----------'   '----------'
     |'               |'
 [===========KAFKA=========]

* drive kibana from tribe node than master
```

---

#### Operational Challenges

* data, lots of it
> transporting, queueing, storing, securing, reliability
> ingesting and indexing fast enough
> scaling infrastructure
> which data?
> formats, mapping, transformation

Kafka takes care for most of it.

Using Kafka-Consul-Consumer can ingest fast enough.

---

#### QnA

* Change default refresh from 5sec to 90sec or so for data to get shown on indexing.

* Kafka Cluster
> 3 Production DataCenter, unique clusters
> 15 to 20 machines each
> 14 Drives in RAID10 configuration
> write heavy, reads low

* Use Logstash's metric filter just to keep tap on it's own health.

* [JTune](https://github.com/linkedin/JTune) JVM Tuning Tool

* For crash during indexing, turn off replication during daily indexing. That way you have replicas for shard failures.

* Kafka to Logstash Pipe.

---
---
