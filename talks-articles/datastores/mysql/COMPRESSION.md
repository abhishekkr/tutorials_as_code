
## MySQL Data Compression

> topics: Page Compression, Table Compression

* If just need compression for a specific field, and table compression isn't suitable or overhead. Opt for app level compression instead.


### Page Compression

> * Supported for `file-per-table` (table in its own .ibd file) tablespaces' tables. A.k.a. `Transparent Page Compression`.
>
> * Requires `sparse file` (filesystem using space efficiently writing metadata on empty blocks to disk) & `hole punching` (release empty page blocks) support. Available at `Win/NTFS`, newer Linux Kernels.

* On Linux, page compression depends on whether compressed data is less-equal than `innodb_block_size - FS_BLOCK_SIZE` (e.g. `16-4=12kb`).

> To enable, usage as below... (page compression setting as zlib, lz4, or None)

```
mysql> CREATE TABLE t1 (c1 INT) COMPRESSION="zlib";

mysql> ALTER TABLE t2 COMPRESSION="zlib";
mysql> OPTIMIZE TABLE t2;
```

> To disable, usage as below...

```
mysql> ALTER TABLE t1 COMPRESSION="None";
mysql> OPTIMIZE TABLE t1;
```
* Querying compressed tables details

```
mysql> SELECT SPACE, NAME, FS_BLOCK_SIZE, FILE_SIZE, ALLOCATED_SIZE FROM
         INFORMATION_SCHEMA.INNODB_TABLESPACES WHERE NAME='tblspc/t1'\G

mysql> SELECT TABLE_NAME, TABLE_SCHEMA, CREATE_OPTIONS
         FROM INFORMATION_SCHEMA.TABLES
         WHERE CREATE_OPTIONS LIKE '%COMPRESSION=%';
```

> * Page Compressiong isn't available for Tables on shared tablespaces (system, temporary, general).
>
> * Not for **undo/redo log tablespaces**, **R-tree pages** used for spatial indexes.
>
> * Pages of compressed tables are left as is.
>
> * Migrating page-compressed tablespaces requires utility that preserves sparse files.


### Table Compression

* ALWAYS NEED TO BE PERF TESTED, WHETHER TABLE COMPRESSION WORKS FOR A TABLE.

> * Not available for InnoDB **System Space** tables.
>
> * Applies to both table data and indexes.

* Can be created in file-per-table (`innodb_file_per_table` be 1) or general tablespace.

* Need `ROW_FORMAT=COMPRESSED` or `KEY_BLOCK_SIZE=` (or both) clauses in `CREATE TABLE` or `ALTER TABLE`.

> If `KEY_BLOCK_SIZE` is given, `ROW_FORMAT=COMPRESSED` is implicit. If just `ROW_FORMAT=COMPRESSED` is given, `KEY_BLOCK_SIZE` is applied half to value of `innodb_page_size`.

```
mysql> SET GLOBAL innodb_file_per_table=1;
mysql> CREATE TABLE t1
         (c1 INT PRIMARY KEY)
         ROW_FORMAT=COMPRESSED
         KEY_BLOCK_SIZE=8;
```

* For General Tablespace, `FILE_BLOCK_SIZE` is must.

```
mysql> CREATE TABLESPACE `ts2` ADD DATAFILE 'ts2.ibd' FILE_BLOCK_SIZE = 8192 Engine=InnoDB;

mysql> CREATE TABLE t4 (c1 INT PRIMARY KEY) TABLESPACE ts2 ROW_FORMAT=COMPRESSED KEY_BLOCK_SIZE=8;
```

> * For General Tablespaces, dropping tables doesn't return disk space nor reduce tablespace's size.
>
> * Providing a `KEY_BLOCK_SIZE > innodb_page_size` gets ignored and half the page size value gets allotted.
>
> * Default uncompressed InnoDB page size is 16kb. Although, for table with many BLOB, VARCHAR, TEXT fields.. setting `KEY_BLOCK_SIZE` to 16 may bring decent results.

* Indexes use the settings from `CREATE/ALTER TABLE` as well; any compression clause provided with `CREATE INDEX` are ignored.

> * General Tablespace can't have mix of compressed/uncompressed tables.
>
> * Compression gets applied to whole table, not individual records.
>
> * InnoDB doesn't support compressed temporary tables.
>
> * Compression Level is same irrespective of `KEY_BLOCK_SIZE`, I/O benefits are what change.
>
> * As data is held in Buffer Pool based on the size (both compressed & its uncompressed copy for changes and updating compressed blocks). So Buffer may need to be tuned, if there are many evictions.

#### When? Why?

* In general suitable for tables way more reads than writes.

* Better for data with enough char strings; and more repeated values than dynamic.

* Check `INFORMATION_SCHEMA` details for monitoring on impact

```
mysql> SELECT * FROM INFORMATION_SCHEMA.INNODB_CMP\G

mysql> SELECT * FROM INFORMATION_SCHEMA.INNODB_CMP_PER_INDEX\G

mysql> SELECT * FROM INFORMATION_SCHEMA.INNODB_CMPMEM\G
```

* More `COMPRESS_OPS_OK` relative to `COMPRESS_OPS` is good. If not can try increasing `KEY_BLOCK_SIZE` for some. For more than 1% failure, compression ain't a good choice.

---
