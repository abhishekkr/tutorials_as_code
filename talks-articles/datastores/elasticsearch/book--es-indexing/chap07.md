
## 07. Snapshot and Restore

### Snapshot Repository

> [current guide](https://www.elastic.co/guide/en/elasticsearch/reference/current/snapshots-register-repository.html)

* register a snapshot

```
curl -XPUT -H "Content-Type: application/json" "localhost:9200/_snapshot/disbakup" -d'{
  "type": "fs",
  "settings": {
    "location": "/data/backups/disbakup",
    "compress": true,
    "chunk_size": "10m"
  }
}'
```

* delete a repository, this only delete reference of snapshot files not the files

```
curl -XDELETE "localhost:9200/_snapshot/disbakup"
```

#### Repository Types

* initially supported shared FS, now supports HDFS, AWS S3, Azure Cloud and more

* shared filesystem repo defined by `fs` value to type can be tweaked for `location`, `compress`, `chunk_size`, `concurrent_streams`, `max_restore_bytes_per_sec` and `max_snapshot_bytes_per_sec`

* `url` repository used to read snapshots created by shared fs supporting `http(s)`, `ftp`, `file` and `jar` protocols; supports wildcard

* cloud support is available via plug-ins


### Snapshot

* a repository can contain multiple snapshot; by default backup all indices

```
## wait_for_completion makes request not return just on initiation
curl -X PUT "localhost:9200/_snapshot/my_backup/snapshot_1?wait_for_completion=true&pretty"
```

* backup selective index/data-streams

> * default `ignore_unavailable` is true to not err at unavailable indices
>
> * `include_global_state` must be false for being able to restore in another cluster
>
> * to avoid failure if any primary shard isn't available set `partial` to false

```
curl -X PUT "localhost:9200/_snapshot/my_backup/snapshot_2?wait_for_completion=true&pretty" -H 'Content-Type: application/json' -d'
{
  "indices": "data_stream_1,index_1,index_2",
  "ignore_unavailable": true,
  "include_global_state": false,
  "metadata": {
    "taken_by": "kimchy",
    "taken_because": "backup before upgrading"
  }
}
'
```

* can obtain info on created snapshot as `curl -XGET "localhost:9200/_snapshot/my_backup/snapshot_1?pretty"`

```
# all snapshots
curl -XGET "localhost:9200/_snapshot/_all?pretty" -H 'Content-Type: application/json"
curl -XGET "localhost:9200/_snapshot/my_backup/_all?pretty" -H 'Content-Type: application/json"

# complete status info of running snapshots
curl -XGET "localhost:9200/_snapshot/_status?pretty" -H 'Content-Type: application/json'
curl -XGET "localhost:9200/_snapshot/my_backup/_status?pretty" -H 'Content-Type: application/json'
curl -XGET "localhost:9200/_snapshot/my_backup/snapshot_1,snapshot_2/_status?pretty" -H 'Content-Type: application/json'
```

* can delete as others `curl -XDELETE "localhost:9200/_snapshot/my_backup/snapshot_2"`


### Restore

* can restore all indices as `curl -X POST "localhost:9200/_snapshot/my_backup/snapshot_1/_restore?pretty"`

* to restore selectively

```
curl -X POST "localhost:9200/_snapshot/my_backup/snapshot_1/_restore?pretty" -H 'Content-Type: application/json' -d'{
  "indices": "data_stream_1,index_1,index_2",
  "ignore_unavailable": true,
  "include_global_state": false,              
  "rename_pattern": "old_(.+)",
  "rename_replacement": "restored_index_$1",
  "include_aliases": false
}'
```

* it errors to restore on an open index, can close as `curl -XPOST "localhost:9200/this_index/_close"`

* to cancel a running Restore operation, can delete index being restored

#### Overriding index settings during restore

* can override most of index settings during restore, like replica count

```
curl -X POST "localhost:9200/_snapshot/my_backup/snapshot_1/_restore?pretty" -H 'Content-Type: application/json' -d'{
  "indices": "index_1",
  "ignore_unavailable": true,
  "index_settings": {
    "index.number_of_replicas": 0
  },
  "ignore_index_settings": [
    "index.refresh_interval"
  ]
}'
```


### How does the snapshot process works?

* data will not be repeated among multiple snapshots of same indices under same backup path

---
