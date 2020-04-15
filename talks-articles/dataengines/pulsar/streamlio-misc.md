
## Streamlio Youtube (Short) Videos on Apache Pulsar

### Concepts and Terminologies

* From ground-up to support multi-tenant use-cases.

* Property is a Tenant. Namespaces are separations under Property. At namespaces (admin unit) can configure replication, permissions, geo-replication, expiry, etc.

* Namespaces (local and global) then have Topics under them. Each topic can have multiple subscriptions.

* Pulsar allows multiple (3 when video was made) kind of subscriptions to co-exist on a topic. Exclusive, Shared and Failover subscriptions.

* Data fed can be from few MBs to multiple TBs in a topic. To handle varied throughput on multiple Topics, thye are sharded across nodes as partitions.

---

### How Apache Pulsar Uses Apache BookKeeper to Store Topics

* When nessages come in they are first send to a journal disk, persisted there. Each individual message is not fsync-d but at time interval. This is not optimized for reading.

* On arrival messages are also inserted to EntryLog, which has a sequential disk file (contiguous for Topic) preceded by Memory Buffer. Offsets for topics for disk files are maintained  in Index entries.

---

### Message Guarantees in Apache Pulsar with Apache BookKeeper

* Guarantess

> * all ACK-ed messages are visible
>
> * all subscribers see all messages
>
> * all subscribers see messages in same order
>
> Total Order Atomic Broadcast, shown to be equivalent to consensus. BookKeeper gets used to provide lock for consistency.

* Guarantees of a Ledger (basic unit of a log), Zookeeper to decide on last entry of a ledger

---

### Apache Pulsar stores Cursors using Apache BookKeeper

* On subscribers' ACK for reading a message, ledger gets updated for that read. Only latest entry of ledger is important.

* There is not a new ledger per subscriber.

---
