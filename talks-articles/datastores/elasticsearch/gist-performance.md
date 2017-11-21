
## Performance

> some collected tips from netverse

* if a cluster has `num_nodes` nodes, `num_primaries` primary shards in total; then to handle `max_failures` node failures at once at most then number of replicas required is `max(max_failures, ceil(num_nodes / num_primaries) - 1)`

---

[tune for indexing](https://www.elastic.co/guide/en/elasticsearch/reference/current/tune-for-indexing-speed.html)

* if only heavy indexing, make `indices.memory.index_buffer_size` large enough to give at most 512 MB indexing buffer per shard doing heavy indexing (beyond that indexing performance does not typically improve)
> default is 10% which is often plenty; if JVM got 10GB then 1GB goes to the index buffer
> you also have `indices.memory.min_index_buffer_size` (defaults 48mb) and `indices.memory.max_index_buffer_size` (defaults unbounded) accepting values like `XXmb`

* disable [_field_names](https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping-field-names-field.html) field indexing if no `exists` query required

* prioritize indexing performance over potential data loss
> `index.translog.durability` to `async`; index will only commit writes to disk upon every `sync_interval` instead after each request

---

[tune for disk usage](https://www.elastic.co/guide/en/elasticsearch/reference/current/tune-for-disk-usage.html)

**disable features you don't need**

* indexing on fields you don't need filter on `{"mappings": {"type": {"properties": {"whateverField": {"type": "integer", "index": false}}}}}`

* if you only need matching capabilities, `text` field store some attributes in index to score documents which are not required
> normalization factors `{"mappings": {"type": {"properties": {"whateverField": {"type": "text", "norms": false}}}}}`
>
> frequencies (for score) and positions (for phrase queries) `{"mappings": {"type": {"properties": {"whateverField": {"type": "text", "index_options": "freqs"}}}}}`
> `docs` have only the doc number indexed, can answer the question if term exist in this field

* no default dynamic string mappings
> typically only `id` field need to be indexed as `keyword` and `body` field as `text`; mark a template to force index as either not both
> like this forces as keyword `{"mappings": {"type": {"dynamic_templates": [{"strings": {"match_mapping_type": "string", "mapping": {"type": "keyword"}}}] }}}`

* large shards will be more efficient at storing data, but long recovery times

* disable `_all` field indexes if never need to search against all fields; deprecated in `v 6.0`
> just know it allows to search for values in documents without knowing which field contains the value
> URI search queires will not be able to use it, can provide alternative field with `index.query.default_field`

* use `best_compression`, but this slows persistence obviously

* use `force merge` and `shrink` api to reduce number of segments

* use smallest generous numeric type for data for significantly improved disk usage
> `byte/short/integer/long` for integer
> `float` for `double` or `half_float` for `float`; or just `scaled_float` for `float`

* [beta] enabled `index sorting` to colocate similar documents; trades time in indexing though

* `v6.0` sparse doc values save 30% to 70% disk space

* `beta v6.0, full v7` mapping types are being deprecated to simplify usage of underlying data structure

---

### Loggly

[9 tips on ES config for high perf](https://www.loggly.com/blog/nine-tips-configuring-elasticsearch-for-high-performance/)

* Loggly is running ES with separate master and data nodes.
> Use separate ES client nodes for both indexing and searching. Takes load off the data nodes, importantly means pipeline can talk to a local client, which then communicates with the rest of the cluster.

* earlier `bootstrap.mlockall: true`; since `v5.x` use `bootstrap.memory_lock: true` to disable swapping
> make sure giving enough heap to ES via `-DXmx` option or `ES_HEAP_SIZE`



[how to monitor ES perf like a pro - logfooding](https://www.loggly.com/blog/how-to-monitor-elasticsearch-performance-like-a-pro-logfooding-part-1/)


---

[qbox.io maximize indexing perf](https://qbox.io/blog/maximize-guide-elasticsearch-indexing-performance-part-1)

* disable `_source` field if data is available elsewhere as well and only desired here for indexed search
> saves disk and can be done by `{"mappings": {"tweet": {"_source": {"enabled": false}}}}`
> although voids auto repair of index corruption, reindex from one ES to another, and such

* pick an ID friendly to lucene
>  which have consistent, sequential patterns that compress well

* consider increasing node level thread pool size, but measure if it brings improvement

* at Lucene a single shard indexing thread limit is 8 but can be increased at ES using `index.index_concurrency`
> can be increased more if single shard a node with single index being indexed into
> though measuring impact while slowly increasing is important

* use dedicated Data nodes
> aggregator node handle search queries giving data nodes more capacity handling indexing requests

---

[elasticsearch performance indexing 2.0](https://www.elastic.co/blog/elasticsearch-performance-indexing-2-0)

* uses adaptive merge IO throttling

* multiple `path.data` paths

* improved ID lookup performance, so auto-id optimization removed; bloom filters removed from defaults for memory consumption issues

---

[performance considerations elasticsearch indexing](https://www.elastic.co/blog/performance-considerations-elasticsearch-indexing)

#### relevant to v 1.3.2 server

* use latest stable build, [Marvel](http://www.elasticsearch.org/overview/marvel) to keep track and optimize

* to begin don't use Java heap larger than half of machine's RAM, leaving other half OS RAM to manage IO caching

* make sure OS is [not swapping out the Java process](https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration-memory.html)

* use SSDs, no remotely mounted filesystem
> EBS options are slower than locally attached SSD
> strip index across multiple SSDs by setting multiple `path.data` directories

* newly indexed documents first kept in RAM by Lucene's `IndexWriter`, periodically flushed to disk segments and eventually many segments merged
> INFO log `now throttling indexing` or segment count growing in `Marvel` hint merges falling behind
> if search during index is less important or on SSDs, disable merge throttle setting `index.store.throttle.type` to `none`

* don't call `optimize` on heavily updated index

* if can accept delay, increase `index.refresh_interval` to `30s` or disable entirely to `-1`; allows larger segments to merge and decrease merge pressure
> it impacts on latest the indexed items are supposed to be searchable

* can increase `indices.store.throttle.max_bytes_per_sec` from `20mb` to `100mb` till `200mb`

* can increase `index.translog.flush_threshold_size` from `512mb` to `1gb` to decrease how frequently fsync got called on index files

* use `0` replicas while building initial large index, then enable replicas later

#### client

* use [Bulk API](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-bulk.html) for indexing multiple documents

* use concurrent bulk requests with client side requests with client-side threads or separate asynchronous requestso

* use tools like iostat, top and ps to confirm you are saturating either CPU or IO across all nodes, if not try more concurrent requests
>  check rejection counts under the `THREAD POOLS - BULK`
* 





---
