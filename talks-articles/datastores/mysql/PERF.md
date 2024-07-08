
## MySQL Performance

* `CREATE`, `ALTER`, `DROP` table/index can be performed ONLINE. So, Supporting Indexes can be added after data load.

* `CHAR(N)` will minimum occupy N space; using `VARCHAR` for columns with multiple NULL alongwith variable length char values is preferred.

* If an Indexed Column is sure to not have NULL, explicitly create with `NOT NULL`. As each query can only use one index.. optimizer can better determine most effective Index to use for a query.

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
