
## Migrating Kafka's Zookeeper without downtime

> [source:yelp engineering blog](https://engineeringblog.yelp.com/2019/01/migrating-kafkas-zookeeper-with-no-downtime.html)

* Kafka uses ZK for varied distributed coordination tasks as electing partition leaders, maintaining metadata about topics

* Here Kafka clusters were migrated to dedicated ZK from central to remove single failure point with too much load

### ZK mitosis

* imitating [cell mitosis](https://en.wikipedia.org/wiki/Mitosis); at a high-level ZK hosts replicate and then divide duplicated hosts into 2 separate clusters using firewall rules

* assume 3 source ZK nodes be `192.168.1.1-3` and destination ZK nodes `192.168.2.1-3`

#### Stage 1: DNA Replication

* add 2 of destination ZK nodes and add to source ZK cluster giving 5 nodes ZK cluster; this automatically performs copy using ZK's replication mechanism

* joint ZK cluster now need a rolling restart, Kafka's config doesn't get updated so it just stays aware of original nodes

* once ZK nodes have data in sync, update Kafka's `zookeeper.connect` config for 2 ZK nodes from new cluster with replicated data

* now rolling restart of Kafka cluster

#### Stage 2: Mitosis

* restore original cluster config for source and destination ZK clusters; DO NOT RESTART ANY NODE YET.

* need destination cluster nodes to drop source cluster nodes; use network rules to split away 2 destination cluster nodes from joint cluster away from 3 source cluster nodes

```
source_node_list="192.168.1.1,192.168.1.2,192.168.1.3"
sudo iptables -v -A INPUT -p tcp -d $source_node_list -j REJECT
sudo iptables -v -A OUTPUT -p tcp -d $source_node_list -j REJECT
```

* now rolling restart on destination ZK cluster (all 3 nodes); forcing them to elect a leader (this step is the time keeping ZK unavailable while ZK starts and leader election happens)

* restart source ZK cluster nodes and clear up network rules on destination cluster nodes

```
source_node_list="192.168.1.1,192.168.1.2,192.168.1.3"
sudo iptables -v -D INPUT -p tcp -d $source_node_list -j REJECT
sudo iptables -v -D OUTPUT -p tcp -d $source_node_list -j REJECT
```


### Building Confidence

* have a regression testing suite ready

* test out procedure on an example setup

* [zk-smoketest](https://github.com/phunt/zk-smoketest) by `github.com/phunt` provides simple smoketest client for ZK ensemble

---
