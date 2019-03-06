
### Some quick ElasticSearch debugging tips


consider `ES_URI` to be something like `http://127.0.0.1:9200`


* get nodes information

```
## hostname, role, free disk, heap used, ram used, FDs used, load
curl -XGET https://escluster/_cat/nodes?v&h=host,r,d,hc,rc,fdc,l
```

* monitor search queues

```
while true; do
  curl -XGET 'host:9200/_cat/thread_pool?v&h=host,search.queue,search.active,search.rejected,search.completed' | sort -unk 2,3 ;
  sleep 5 ;
done
```

* check indices

```
curl -XGET https://escluster/_cat/indices?v
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

* recovery information

```
curl -XGET https://escluster/_recovery?pretty&active_only
```

* cluster stats

```
curl -XGET https://escluster/_cluster/stats?pretty
```

* node stats

```
curl -XGET https://escluster/_nodes/stats?pretty
```

* all cluster settings

```
curl -XGET https://escluster/_settings
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
