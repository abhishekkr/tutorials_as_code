
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

---
