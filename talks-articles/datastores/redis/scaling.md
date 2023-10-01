
## Redis Scaling

* Replica Shards for Read op to load-balance.

> * Replicas of Replicas are allowed, to have less load on primary for syncs as well.
> * Default is async replication, non-blocking.
> * Primary must have persistence on. Replica must have read-only.

* Redis Cluster provides a way for data automatically sharded for r/w, using hashtag buckets.

> * Cluster nodes use a different ClusterPort for intra-comm.
> * Re-sharding happens on add/del of nodes. Secondary shards can promote to primary on their failure.
> * Clients can select correct master to write to, when sharded. Or can use Proxy assisted partitioning.

---
