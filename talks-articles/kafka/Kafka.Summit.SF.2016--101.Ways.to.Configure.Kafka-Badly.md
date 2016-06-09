## 101 Ways to Configure Kafka - Badly
> by Henning @spjelkavik, Audun @audunstrand (Finn.no)
> at Kafka Summit - SF -2016

* different clusters for different requirement, tuned for required throughput and reliability

---

* Too many parititions per topic

Partitions are Kafka's way of scaling consumers. 128 partitions can handle 128 consumers.

> Many Partitions and many topics creates massive need for coordination which gets handled by ZK.

In 0.8, clusters could not reduce the number of partitions without deleting data.

Highest number of consumers today is 20.

> Only increase partitions for selected topics. Reduce length of transactions on consumer side.
> Max partitions on a broker advised is 1500.

---

* 8 Kafka Nodes, 8 ZK Nodes

ZK is dependent on a majority quorum, low latency between nodes.
8 nodes were NOT dedicated, quite overloaded.

ZK recommends 3 nodes for normal usage, 5 for high, more is questinable.
More nodes lead to more communication. Splitting them between DataCenters is bad idea cuz of latency and outage cases.

ZK needs odd number nodes for quorum.

> Also don't run sensitive services (ZK) on a server with too many jvm-based service having over-committed RAM. GC.

---

#### References

* [Wiki](https://cwiki.apache.org/confluence/display/KAFKA/Kafka)

* [Kafka Paper and Presentations](https://cwiki.apache.org/confluence/display/KAFKA/Kafka+papers+and+presentations)

---
