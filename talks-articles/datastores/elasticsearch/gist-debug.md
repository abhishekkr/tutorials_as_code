
### Some quick ElasticSearch debugging tips


consider `ES_URI` to be something like `http://127.0.0.1:9200`


* check cluster health

```
curl -skL -XGET "${ES_URI}/_cluster/health?pretty"
```


* check state of all shards across all nodes

```
curl -skL -XGET "${ES_URI}/_cat/shards"
```


* retry all failed routing of unassigned shards

```
curl -skL -XPOST "${ES_URI}/_cluster/reroute?pretty&retry_failed"
```


* move shards to some place else

```
curl -XPOST '${ES_URI}/_cluster/reroute?pretty' -H 'Content-Type: application/json' -d'
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
