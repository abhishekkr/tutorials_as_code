### Qik DataStore Insight

> sources:
> * [9DBs in 45minutes](https://youtu.be/XfK4aBF7tEI)

---

PostgreSQL
* Ingress -> Post-Ingress -> PostgreSQL -> open-source
* HStore : Key/Val
* 18 language support for Stored Procs including Pl/V8 (javascript)

---

CouchDB : Cluster Of Unreliable Commodity Hardware
* nosql, document, provides MapReduce, in Erlang

---

Marklogic
* old, secure, nosql, used in US defense

---

Riak
* implementation of DynamoDB
* have HTTP Rest and Protobuf interface
* default is last-write wins, vector clocks to decide last write
* full-text search add-on inspired by Lucene

---

VoltDB
* newSQL RDBMS, commercialized 'HStore', GPL3 Community Edition also there,
* in-mem, sql/java stored procs, single threaded
* linear scaling, built-in horizontal replication
* full ACID, high velocity, transactional consistency

---

MongoDB
* name from huMONGOus as aiming for Big Data
* document oriented, json/bson, operator support
* mapreduce

---

Neo4J
* embedded disk-based full-transactional java persistence engines that stores in graphs
* java/REST/Cypher/Gremlin

---

HBase
* implementation Google BigTable
* Columnar Store
* strict consistency; distributed on commodity hardware
* no data-types; everything is byte
* connection via shell, java-api, thrift, rest, avro
* standalone mode, pseudo-distributed mode and full distributed mode; recommendation at least 5nodes

---

Redis
* REmote DIctionary Service
* many utility features, data structures
* PubSub and Blocking Queues
* Scriptable in Lua
* In-Memory

---
---
