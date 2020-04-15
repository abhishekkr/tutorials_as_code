
## Short Intro notes from [pulsar.apache.org](https://pulsar.apache.org/docs/en)

### Pulsar Connector Overview

* enable to easily create, deploy and manage connectors that interact with external systems to be treated as `source` or `sink`

* Sources such as Canal, Demezium MySQL/PgSQL, File, Flume, Twitter Firehose, Kafka, Kinesis, Netty, RabbitMQ

* Sinks such as Aerospike, Cassandra, ElastiSearch, Flume, HBase, HDFS2, HDFS3, InfluxDB, JDBC, Kafka, Kinesis, MongoDB, RabbitMQ, Redis, Solr

---

### Tiered Storage

* Backlog can grow very large with segment oriented architecture.

* With tiered storage, older messages in backlog can be moved from BookKeeper to a cheaper storage mechanism.

* It still allows clients to access backlog.

* S3, Google Cloud Storage and File can be used.

---
