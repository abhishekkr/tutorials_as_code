
## MySQL Performance

* `CREATE`, `ALTER`, `DROP` table/index can be performed ONLINE. So, Supporting Indexes can be added after data load.

* `CHAR(N)` will minimum occupy N space; using `VARCHAR` for columns with multiple NULL alongwith variable length char values is preferred.

* If an Indexed Column is sure to not have NULL, explicitly create with `NOT NULL`. As each query can only use one index.. optimizer can better determine most effective Index to use for a query.

### MySQL Settings

* Config `innodb_buffer_pool_size` to start at 80% of RAM; then tune based on monitoring.

* Config `innodb_log_file_size` as 1G; for very high write intensive as 4G or more.

* Config `innodb_file_per_table= ON` allows easy reclaim of space when dropping/optimizing/compressing tables. Bad if have say have 10k+ tables. Is default ON, v5.6 onwards.

* Config `max_connections` can be increased; but high values (say 1K+) may leave server unresponsive. Better to also check for services connections open.. and use connection pool.

* Config `innodb_flush_log_at_trx_commit = 1` means fully ACID. Setting it to `2` is a bit less reliable as committed TX will get flushed to redo logs only 1/sec. So a good value for replica; maybe acceptable for primary based on usecase.

* If the `innodb_log_waits` status is not 0, increase `innodb_log_buffer_size`.

* Make `query_cache_size = 0` (default since v5.6). Instead use indexing, optimize queries, add read replicas, use external Memcache/Redis.

* Have `expiry_log_days` set for binary logging. Binary logging is heavy, so only use if needed.

* Have `skip_name_resolve` disable DNS lookups for hostname resolution on client connections. Would require IP addess in GRANT statements.


### Optimize Table

> * `OPTIMIZE TABLE MYTABLE`;
> * usable for table with high read/delete records;
> * to be used only with enough diskspace

* Can be used to **defragment** table or to update InnoDB fulltext index. So less storage & better I/O.

> * Defragments Innodb tables as created with `innodb_file_per_table` option enabled.
> * FULLTEXT index in InnoDB, need first `innodb_optimize_fulltext_only=1`. Keep maintenance period managed with `innodb_ft_num_word_optimize` option.

* Should be regular.

* Works for InnoDB, MyISAM, and ARCHIVE tables. Works for Partitioned Tables.
> * Post MySQL 5.6.17.. OPTIMIZE TABLE is performed online for regular and partitioned InnoDB tables.
> * XLOCK is taken.. during prepare phase for metadata update and an intermediate table is created. Then during commit phase, table metadata changes to be committed.


### Buffering & Caching

#### InnoDB Buffer Pool Optimization

> Buffer pool size must always be equal to or a multiple of `innodb_buffer_pool_chunk_size * innodb_buffer_pool_instances`.

```
mysql> SELECT @@innodb_buffer_pool_size;
mysql> SELECT @@innodb_buffer_pool_chunk_size;
mysql> SELECT @@innodb_buffer_pool_instances;
```

* Buffer Pool state can be saved and restored. Popular use-case is reduce restart warm-up; also reset buffer after running report/maintenance tasks to rid of less required state.

* InnoDB stores caching data & indexes in memory. `SHOW ENGINE INNODB STATUS` to see impact.

* For Linear read-ahead `innodb_read_ahead_threshold` value `0-64; default: 56`. So if sequential pages read is more than it; async prefetch happens.

* Ranom read-ahead `innodb_random_read_ahead` to ON makes InnoDB async prefetch pages based on prediction regardless of order of pages in buffer pool.

* `innodb_page_cleaners` (auto set to value `innodb_buffer_pool_instances`) value `1-64; default: 4` flushes dirty pages from buffer pool instances. May impact write-io.

* If have buffer pool in GBs; dividing pool instances improves **concurrency**; configured with `innodb_buffer_pool_instances` (value `1-64; default: 1`) with adjusted `innodb_buffer_pool_size`.
> Each pool has its own free/flush lists, LRUs, mutex, etc.

* In periodic batch reporting queries which result in large scans, setting `innodb_old_blocks_time` (default: 1000; higher value ages blocks quicker) during runs help keep normal workload in the buffer pool.

* When scanning large tables that cannot fit entirely in the buffer pool, setting `innodb_old_blocks_pct` to a small value (default: 37; like 5 restricts this read once data to 5% of the buffer pool).


#### Query Cache (Deprecated by v5.7.20; removed by v8)

> Better to use options like ProxySQL+Readyset now, if need be.

* Query cache stores `SELECT` statements with retrieved records in memory.

```
mysql> show variables like 'have_query_cache';
mysql> show variables like 'query_cache_%' ;

-- query_cache_type => 0: OFF; 1: Cache all except 'SELECT SQL_NO_CACHE'; 2: Only cache 'SELECT SQL_CACHE'
-- important stats are 'Qcache_lowmem_prunes', 'Qcache_free_memory', 'Qcache_hits'
```

* To check query durations

```
mysql> SET profiling = 1;
mysql> SHOW PROFILES;
-- result would show Query IDs; for details can do 'SHOW PROFILE FOR QUERY 1;'
```

#### Cache Prepared Statement

* MySQL's Prepare allows have a statement once; run multiple times with different parameters. Server converts statement into internal structure & caches it; to avoid overhead of reconverting on each execution.. thus more efficiency.
> * MySQL's stored procedures, functions, triggers & events are similarly cached as well.
> * These caches are session specific.
> * MySQL keeps cache updated to DDL changes. The `Com_stmt_reprepare` status variable tracks the number of repreparations.

> Also available from within ORMs, as Go's [GORM](https://gorm.io/docs/performance.html#Caches-Prepared-Statement).

---
