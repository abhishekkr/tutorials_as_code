
### Some quick ElasticSearch debugging tips


consider `ES_URI` to be something like `http://127.0.0.1:9200`

* [REST APIs official doc](https://www.elastic.co/guide/en/elasticsearch/reference/current/rest-apis.html)


* get nodes information

```
## hostname, role, free disk, heap used, ram used, FDs used, load
curl -XGET "${ES_URI}/_cat/nodes?v&h=host,r,d,hc,rc,fdc,l"
```

* monitor search queues

```
while true; do
  curl -XGET "${ES_URI}/_cat/thread_pool?v&h=host,search.queue,search.active,search.rejected,search.completed" | sort -unk 2,3 ;
  sleep 5 ;
done
```

* check indices

```
curl -XGET "${ES_URI}/_cat/indices?v"

curl -X GET "${ES_URI}/_cat/indices?bytes=b&s=store.size:desc&v&pretty"
```


* check cluster health

```
curl -skL -XGET "${ES_URI}/_cluster/health?pretty"
```


* check state of all shards across all nodes

```
curl -skL -XGET "${ES_URI}/_cat/shards?v"
```


* retry all failed routing of unassigned shards

```
curl -skL -XPOST "${ES_URI}/_cluster/reroute?pretty&retry_failed"
```


* move shards to some place else

```
curl -skL -XPOST "${ES_URI}/_cluster/reroute?pretty" -H 'Content-Type: application/json' -d'
{
    "commands" : [
        {
            "move" : {
                "index" : "shard.name.xyz",
                "shard" : 0,
                "from_node" : "node1",
                "to_node" : "node2"
            }
        },
        {
          "allocate_replica" : {
                "index" : "shard.name.xyz",
                "shard" : 0,
                "node" : "node3"
          }
        }
    ]
}
'
```

* recovery information

```
curl -XGET "${ES_URI}/_recovery?pretty&active_only"
```

* cluster stats

```
curl -XGET "${ES_URI}/_cluster/stats?pretty"
```

* node stats

```
curl -XGET "${ES_URI}/_nodes/stats?pretty"
```

* all cluster settings

```
curl -XGET "${ES_URI}/_settings"
```


---

### deleting indices

* delete all

```
curl -X DELETE '${ES_URI}/_all'
```

* delete an index

```
curl -X DELETE '${ES_URI}/shop'
```

---

### mappings

* all mappings

```
curl -X GET '${ES_URI}/_mapping'
```

* specific indexes mapping

```
curl -X GET '${ES_URI}/my-index-01/_mapping'

curl -X GET '${ES_URI}/my-index-01,my-index-02/_mapping'
```

---
