
## PostgreSQL Tricks

> source: [dbrnd](https://www.dbrnd.com/2019/06/postgresql-disable-non-durable-parameters-and-improve-server-performance/)

### Disabling durability parameters for performance if acceptable, not for production

* `fsync`: Turn off; no need to flush data to disk

* `synchronous_commit`: Turn off; not need to write to disk on every commit

* `full_page_writes`: Turn off; no need to guard against partial page writes

* `max_wal_size`: Increase; reduces the frequency of checkpoints

* `checkpoint_timeout`: Increase; reduces the frequency of checkpoints

---
