
## 02. Whats is an Elasticsearch Index

### Nature of ES Index

* distributes data to more than one Lucene index as physical (shard) by default

* `mapping` defines how a document be indexed, target query be analyzed

* `types` were categorization within an index, but are slowly deprecated to do away with their misuse

> goal of types was multi-tenancy within index, which doesn't suit Lucene, v8 will completely remove APIs accepting types

* can use comma-separated index names for multiple indices search

```
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime,example_idx/_search?pretty"
```

### Document

* it's schema-less, thus can process a new index & field without pre-notification


### Denormalization

* optimizing read perf by adding redundant data; with a signle document containing all vectors there is no need to join

---
