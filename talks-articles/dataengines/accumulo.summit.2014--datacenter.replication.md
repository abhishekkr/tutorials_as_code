
> by John Else
> at accumulo summit 2014

## Data-Center Replication with Apache Accumulo

> cuz never trust hardware

### Implementation

* framework of tracking data written to a table
* interfaces to transit data to another storage system
* async replication, eventual consistency
* configurable cyclic replication graph
* `primary-push` system from primary to peers
* survives prolonged peer outages

* WAL is source of data for replication

* Primary storage of book-keeping in a new table

* Requires some records in Accumulo metadata table

* Pluggable replication work assignment by Master to TabletServers on primary
> default implementation uses ZK

---

### Replication

* AccumuloReplicaSystem, runs inside primary tserver and sends data t peer tserver using Thrift.

---

### Future

* replication to other types of systems
> nosql, rdbms

* support of replication bulk imports

* support of conditional mutations

* consistency of table config

---
---



















---
---
