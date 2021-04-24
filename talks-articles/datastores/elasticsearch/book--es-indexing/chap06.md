
## 06. Improving Indexing Performance

### Configuration

#### Memory Configuration

* use `ES_JAVA_OPTS` env-var to provide JVM settings for memory

> `-Xmx` for max heap allowed, `-Xms` for initially allocated heap-size, `-Xmn` gives initial and max heap size for young generation as bytes
>
> JDK 8 Hotspot JVM now uses native memory; it's called Metaspace and removed permanent generation by through-put GC

* use `ES_HEAP_SIZE` env-var to configure `-Xmx` and `-Xms`

* to check JVM settings of each node

```
curl -X GET "localhost:9200/_nodes/jvm?pretty"
```

#### Avoid Swapping

* to avoid bad consequence of swap operation, `bootstrap.mlockall: true` property can be enabled to lock heap memory

* this config can be checked at `curl -X GET "localhost:9200/_nodes/process?pretty"`

#### GC and Structure of JVM Memory

* 5 parts grouped under 2 main groups called `Young Generation` and `Old Generation`

> * `Eden Space`: part of heap initially allocated for most object types
>
> * `Survivor Space`: part of heap that survived GC of Eden space, it's further divided into `survivor space 0` and `survivor space 1`
>
> * `Tenured Generation`: part of heap that holds objects that have existed for some time in Survivor Space
>
> * `Permanent Generation`: non-heap part that holds all data for VM itself
>
> * `Code Cache`: non-heap part in HotSpot JVM; used for compilation and storage of native code

* here Eden Space and 2 Survivor Space make up the Young Generation; when Eden Space is filled `minor GC` is performed

* every minor GC; survivor objects are checked and moved to Survivor Space.. from where they graduate to Tenured Space

* Old Gen `major GC` is applied when application no longer needs an object here or space is filled

##### What is the problem?

> * when GC runs, all app-threads are frozen
>
> * `minor GC` has short-lived objects so generally doesn't cause trouble
>
> * but `major GC` deals with long-living objects so higher frequency of it might cause timeout errors

##### Monitoring GC

> * can use commands like `jstat` at console or `jconsole` for UI
>
> * can even use `VisualVM` with `Visual GC` plug-in

* Process of deallocating memory consists 3 steps:

> * Marking: GC identifies which objects are in use and which not
>
> * Normal Deletion: GC cleans unreferenced objects
>
> * Deletion with Compacting: all reference objects can be moved together, making newer allocation contiguous and faster

##### Types of GC

* Serial GC `-XX:+UseSeialGC`,designed for single threaded env; while it runs entire app-thread set is frozen

* Parallel GC `-XX:UseParallelGC`, the through-put collector uses multiple threads of Old Gen GC but still freezes the world

* Concurrent Mask Sweep GC `-XX:UseConcMarkSweepGC`, uses multiple threads to scan heap.. suitable where need to avoid long pauses

* G1 GC `-XX:+UseG1GC`, is a server style GC targetted for multi-processor machines. Separates heap into multiple equal reqions & does parallel collection. This doesn't have young and old gen concepts.

##### Tuning the GC

* for `OutOfMemory` error, try increasing `PermGen` memory space or can use `-XX:+CMSClassUnloadingEnabled` option to sweep PermGen and remove unused classes


#### File Descriptors

* should increase FDs for ES user to at least 32K or 64K

* can temporarily set via `sysctl -w vm.max_map_count=1000000`; change it in `/etc/sysctl.conf`

* can query available FDs to ES at `max_file_descriptors` key in result of `curl -X GET "localhost:9200/_nodes/process?pretty"`

* if JVM doesn't pick increased count, try using `-XX:-MaxFDLimit`


### Optimization of Mapping Definition

#### Norms

* score indicates how well a document matchs a query; `norms` are a vector of it

* Lucene accounts for `field length`; searched term found in less content is more tied together than in larger content

* `norms` store various normalization factors like this, and useful for searching but use disk

* can be disabled as following, but not re-enabled

```
curl -X PUT "localhost:9200/my_index/_mapping?pretty" -H 'Content-Type: application/json' -d'{
  "properties": {
    "title": {"type": "text", "norms": false}
  }
}'
```

> norms are not removed instantly, but over time as old segments merge into newer

#### Feature `index_option` of string type

* controls what info is added to inverted index for search and highlighting

* parameters

> * `doc`: doc numbers are indexed
>
> * `freqs`: doc numbers & term frequencies
>
> * (default) `position`: doc numbers, term frequencies & term positions
>
> * `offsets`: doc numbers, term freq. & positions alongwith start & end char offsets (used by `unified highlighter`)

```
## will not count times a term appears
## phrase & proximity query are not available
curl -XPUT -H "Content-Type: application/json" "localhost:9200/my_index?pretty" -d'{
  "mappings": {
    "properties": {
      "phone_number": {
        "type": "keyword",
        "index_options": "docs"
      }
    }
  }
}'
```

#### Exclude unnecessary fields

* can exclude storage/indexing of some fields to reduce the load

#### Extension of automatic index refresh time

* ES uses FS caches to overcome I/O bottleneck

* writing and opening a new segment is called Refresh (happens almost per second), supporting real-time search

* can set `index.refresh_interval: 2s` in `elasticsearch.yml` to set automatic refresh value for all indices

* can set value per index as `curl -XPUT -H "Content-Type: application/json" "localhost:9200/my_index/_settings" -d '{"index": {"refresh_interval": "5s"}}'`

* can even turn-off automatic refresh by setting value to `-1` (not suggested unless extreme case)

> remember that data reflection is delayed in this case


### Segments and Merging Policies

* Lucene index is composed of smaller fully-independent chunks called `segments`

* every segment is checked at search; so more the segments, more cpu/memory used to search them

* segments get merged to resolve resource issue; happens async in background while indexing/searching

* `searchable segment` are on-disk Lucene segment marked as searchable; `committed segment` are searchable segments that have been fsynced/flushed to disk so less risk of loss (also cleared of translog); `uncommitted segment` exist only in translog and not yet committed as Lucene segment

* old segments are dropped/deleted, when relevant documents are merged to newer segment

* to check how many deleted documents, mem use, disk space use & other details

```
curl -XGET "localhost:9200/twitter/_segments?pretty"
```

* `index.merge.scheduler.max_thread_count` be `1` on spinning platter disk, can set to `Math.max(1, Math.min(4, <<node.processors, node.processors>> / 2))` for SSDs

#### Choosing right merging policy

* can control which segments of a shard index are merged by `index.merge.policy.type` in `elasticsearch.yml`

* type `tiered` (default): calculates how many segments are allowed in index by `budget`, finds low-cost merge checking less size with delete documents

> * `index.merge.policy.expunge_deletes_allowed`, only merges a segment if delete % is over threshold (default: 10)
>
> * `index.merge.policy.floor_segment`, avoid frequent flushing of tiny segments (default: 2MB)
>
> * `index.merge.policy.max_merge_at_once`, specifies max segments merged at time during normal merge (default: 10)
>
> * `index.merge.policy.max_merge_at_once_explicit`, specifies max segments merged at time during optimize operation (default: 30)
>
> * `index.merge.policy.max_merged_segment`, max target single segment that can be produced (default: 5GB)
>
> * `index.merge.policy.segments_per_tier`, number of segments per tier (default: 10).. needs to be higher than `max_merge_at_once` to avoid too many merge
>
> * `index.merge.policy.reclaim_deletes_weight`, specifies how aggresively merges reclaiming deletes are favored (default: 2.0)
>
> * `index.compound_format`, is boolean or float specifying whether index be stored in compound format (default: false); if true Lucene will build index in single file

* type `log_byte_size`: similar to tiered merges segments into levels of exponentially increasing `byte_size`; this is to be deprecated


#### The ForceMerge API

* query param `only_expunge_deletes` defaults to false, if true only expunge segments with deletion

* param `flush` to force flush after merge

* param `max_num_segments`, to fully merge all set it to 1

```
## for specific index
curl -XPOST -H "Content-Type: application/json" "localhost:9200/twitter/_forcemerge?pretty"

## for all indices
curl -XPOST -H "Content-Type: application/json" "localhost:9200/_forcemerge?pretty"
```


### Store Module

> control how index data is stored

#### Store Types

* ES automatically picks best for OS env, can override the config in `elasticsearch.yml` with `index.store.type: niofs` or `curl -H "Content-Type: application/json" -XPUT "localhost:9200/twitter" -d '{"settings": {"index.store.type": "niofs"}}'`

* `fs` (default), picks best for OS env which is `hybridfs` generally that uses `MMap` (term dict, norms, doc values) and `Java NIO` depending on file-type

* `simplefs` a straight implementation of FS storage mapping to Lucene `SimpleFsDirectory` using random access file

* `niofs` stores shard index using `java.nio` mapping to `LuceneNIOFSDirectory`

* `mmapfs` stores shard index at Lucene `MMapDirectory` by mapping a file into memory


#### Throttling I/O Operations

* segments written once and immutable up to delete markers

* store module allows throttling for write/merges on a node/index level

* [index recovery settings](https://www.elastic.co/guide/en/elasticsearch/reference/current/recovery.html) allows control recreation/relocation of shards across nodes from primary shards while managing max bytes per sec, max chunks


### Bulk API

* allows perform multiple index/delete calls in single API

```
curl -XDELETE "${ES_URL}/my_index"
echo "" && sleep 1

curl -XPOST "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "index" : { "_index" : "my_index", "_id": 1 } }
{"title": "First Post :)", "content": "start wiriting now!!"}
{ "index" : { "_index" : "my_index", "_id": 2 } }
{"title": "today is a your'\''s", "content": "Hey! :( well it should have been a better day."}
{ "index" : { "_index" : "my_index" } }
{"title": "shall we write", "content": "I guess, x( that is why we are here."}
'

curl -XPUT "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "create" : { "_index" : "my_index", "_id": 11 } }
{"title": "eleven", "content": "iele won"}
{ "create" : { "_index" : "my_index", "_id": 12 } }
{"title": "twelve", "content": "just kick this."}
{ "create" : { "_index" : "my_index", "_id": 13 } }
{"title": "thirteen", "content": "ok in the teens."}
'

curl -XPUT "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "update" : { "_index" : "my_index", "_id": 11 } }
{"doc": {"title": "eleven-th", "content": "iele won"}}
{ "update" : { "_index" : "my_index", "_id": 12 } }
{"doc": {"title": "twelve-th", "content": "just kick this."}}
'

curl -XPUT "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "delete" : { "_index" : "my_index", "_id": 12 } }
{ "delete" : { "_index" : "my_index", "_id": 13 } }
'

echo "" && sleep 1

curl -XGET "localhost:9200/my_index/_mapping?pretty"

curl -XGET "localhost:9200/my_index/_doc/11?pretty" -H 'Content-Type: application/json'
curl -XGET "localhost:9200/my_index/_doc/12?pretty" -H 'Content-Type: application/json'
```

#### Bulk Sizing

* no perfect value, slowly increase from 5MB to get good value for your nodes

---

