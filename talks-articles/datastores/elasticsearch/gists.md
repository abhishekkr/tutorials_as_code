
## Snippets for ES

* mass optimize, indexes with most deleted docs first

```
for indice in $(CURL -XGET esmaster:9200/_cat/indices | sort -rk 7 | awk '{print $3}'); do
  curl -XPOST http://esmaster:9200/${indice}/_optimize?max_num_segments=1
done
```


* restart a cluster with rack awareness

```
curl -XPUT 'host:9200/_cluster/settings' \
  -d '{ "transient" : { "cluster.routing.allocation.enable": "none" }}' ;

for host in $(curl -XGET esmaster:9200/_cat/nodeattrs?attr | awk '/rack_id/ {print $2}'); do
  ssh $host service elasticsearch restart
done

sleep60

curl -XPUT 'host:9200/_cluster/settings' -d '{ "transient" : { "cluster.routing.allocation.enable": "all }}'
```


* optimize cluster restart

```
### once master is back
curl -XPUT 'http://escluster:9200/_cluster/settings' -d '{
 "transient" : {
 "cluster.routing.allocation.cluster_concurrent_rebalance": 20,
 "indices.recovery.concurrent_streams": 20,
 "cluster.routing.allocation.node_initial_primaries_recoveries": 20,
 "cluster.routing.allocation.node_concurrent_recoveries": 20,
 "indices.recovery.max_bytes_per_sec": "2048mb",
 "cluster.routing.allocation.disk.threshold_enabled" : true,
 "cluster.routing.allocation.disk.watermark.low" : "90%",
 "cluster.routing.allocation.disk.watermark.high" : "98%",
 "cluster.routing.allocation.enable": "primary"
 }
}'

### once cluster is back to yellow
curl -XPUT 'http://escluster:9200/_cluster/settings' -d '{
 "transient" : {
 "cluster.routing.allocation.enable": "all"
 }
}'

### remove data nodes from a cluster without getting yellow
curl -XPUT 'http://escluster:9200/_cluster/settings' -d '{
 "transient" : {
 "cluster.routing.allocation.exclude._ip" : "<data node 1>,<data node 2>,<data node x>"
 }
}'
```


---
