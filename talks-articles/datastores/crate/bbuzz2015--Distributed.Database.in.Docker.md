## Understanding Databases for Distributed Docker Applications
> by Chris Ward @chrischinch
> at Berlin Buzzword 2015

[crate.io](https://crate.io), @crateio, `#crate` on freenode

### Crate?

* built on a NoSQL arch
* a distributed SQL DB
* supports semi-structured records
* superfast, powerful search
* horizontal scaling, elastic, resilient
* eventual consistent, high concurrency
* easy setup

---

### What it builds upon

```
------------------------------------------------------------
            | [+ Crate  ] [+ Crate] [+ Crate  ] [+   ]
Client      | [Dashboard] [  Shell] [Libraries] [Java]
------------------------------------------------------------
            | [@ FB Presto] [$ Scatter/] [+ Query] [+   Bulk     ]
Query       | [ SQL Parser] [  Gather  ] [Planner] [Import/Export]
------------------------------------------------------------
            | [+ Distributed] [+ Distributed] [+ Data Transformation]
Aggregation | [     SQL     ] [    Reduce   ] [ & Reindex Support   ]
------------------------------------------------------------
            | [@    ] [$ Transport] [$ Discovery] [$       ] [+ BLOB   ]
Network     | [Netty] [  Protocol ] [  & State  ] [Sharding] [Streaming]
------------------------------------------------------------
            | [@     ] [$            ] [+  BLOB]
Storage     | [Lucene] [Elasticsearch] [Storage]
------------------------------------------------------------

 + : crate's own components
 @ : other components
 $ : elasticsearch components

```
---

## Quick Demo via Docker

```
docker pull crate:latest
docker run -d -P crate
docker run -d -P crate
docker run -d -p 4200:4200 -p 4300:4300 crate:w

docker ps
```

now that services are up and one is listening at 4200,4300
* browse up the admin console at `http://127.0.0.1:4200/_plugin/crate-admin`
* can go to 'Cluster' tab, check all nodes are there
* can go to  'Get Started' tab and pull soem random tweet data
* can go to 'Tables' tab to check on what's there
* can go to console and play around SQL on available tables

> **Data might not be available immediately.** Till syncs.

#### For Crate dockers spread across multiple hosts

to extend Crate instance
Service Discovery between the nodes generally work when everything is on same host.
So following way can be opted to explicitly update service discovery.
Or as later mentioned, something like Weave to provide network overlay, for real Zero-Config.

```
PRIVATE_IP=$(curl http://169.254.169.254/latest/metadata/local-ipv4)

HOSTS="crate01.node:4300,crate02.node:4300,crate03.node:4300,crate04.node:4300"


docker run -d -p 4200:4200 -p 4300:4300
  --volume /data:/data
  --env CRATE_HEAP_SIZE=8g
  crate crate
  -Des.path.data="/data/data1,/data/data2"
  -Des.multicast.enabled=false
  -Des.network.publish_host=$PRIVATE_IP
  -Des.discovery.zen.ping.unicast.hosts=$HOSTS
```
the data is already there

---

### Integrating in Apps

Can be used via libraries written in Java, Javascript, Python, PHP, Ruby.
For languages with no native libraries, can use REST directly.

---

### For Microservice supporting set-up, Docker Compose

```
crate:
  image: crate
  ports:
    - "4200:4200"
    - "4300:4300"
  volumes:
    - /mnt/data/crate:/data
  environment:
    CRATE_HEAP_SIZE: 16g
  command: crate -Des.cluster.name=my-crate -Des.node.name=crate-1 -Des.network.publish_host=cratedemo.dev

node:
  build: .
  ports:
    - "8000:8000"
  links:
    - crate
```

---

### Docker Machine and Swarm

* docker-machine for more customizable docker usage for backend, etc

```
docker-machine create --driver virtualbox staging

eval "$(docker-machine env staging)"

docker run -d -p 4200:4200 -p 4300:4300 crate
```

* creating discovery token of master through the swarm,

```
docker-machine create -d virtualbox env-crate

$(docker-machine env env-crate)

docker run swarm create

echo "export TOKEN=xx" >> .bash_profile

source .bash_profile
```

* start creating other nodes

```
docker-machine create --driver virtualbox --swarm --swarm-master --swarm-discovery token://${TOKEN} crate-swarm
```

```
docker-machine create --driver virtualbox --swarm --swarm-discovery token://yy crate-swarm-node1
```

```
docker-machine ls

$(docker-machine env --swarm crate-swarm)

docker info
```

* for any cloud with instances on different host, will end using something like

```
docker -H tcp://HOST:2376
  run -d -p 4200:4200 -p 4300:4300
  crate:latest crate
  -Des.cluster.name=crate-swarm
  -Des.multicast.enabled=false
  -Des.transport.publish_host=HOST
  -Des.discovery.zen.ping.unicast.hosts="HOSTS"
  -Des.discovery.zen.minimum_master_nodes=x
```

unless use a smart solution like **Weave**

---

[Crate + Weave](https://crate.io/blog/crate-with-docker-and-weave)

Make Crate work without Host hack, feeling running on same host.
Docker support.
SDN providing overlay network over other networks.
Supporting multicast to work in most clouds.

---

### Use Cases

##### Well Suited

* high volume, semi-structured/dynamic data
* powerfull fulltext search
* elastic datastore
* real-time analytics and BI
* eventual consistency

##### Not Well Suited

* systems that require strong consistency
* systems requiring transactions
* strong realtional data

> no joins as of now, coming...

---

## Architecture

### distributed real-time SQL

* automatic sharding, partitioning, replication
* optimistic concurrency control, read-after write consistency
* aggregations are superfast, executed truly distributed by realtime Map/Reduce
* crate uses standard SQL and can handle 1000s of r/w connections per node
> equi-JOINs to be released soon (talk year 2015)

### shared-nothing

* nodes don't share state
* all nodes are equal
* each node is independent and self-sufficient
* each node can perform every task

```
   Just add/rem nodes, 0-config, self-healing
  [ |]--[ |]--[ |]--.....................-[ |]
   |
   |
   |            All nodes are equal
 ,-------------------------------------------,
 |                             Handlerside   |
 |  [=HTTP=]--[=Transport=]---------------,  |
 |                 |                      |  |
 |            [   Parser               ]  |  |
 |                 |                      |  |
 |            [ Analyzer/Planner       ]  |  |
 |                 |                      |  |
 |            [ Distribution/Execution ]  |  |
 |                 |                      |  |
 |               Transport                |  |
 |                                        |  |
 |      |                  |              |  |
 |   Collect.Node     Merge.Node   Blob.Node |
 |      |                  |              |  |
 |      '-----Data---------'             FS  |
 |___________________________________________|

```

### horizontal scaling

* quantity over quality
* increase amount of (smaller) nodes instead of scaling a single node
* distributed/parallel computation power
* new way building webservices

```
     [slave]     [slave]     [slave]  |m|  | [Crate]     [Crate]     [Crate] |
       \            |          /      |a|  |   \            |          /     |
       [=======DB=Master=======]      |p|  '---------------------------------'
         /          |         \       | |       |          |          |
  [AppServer] [AppServer] [AppServer] |t| [AppServer] [AppServer] [AppServer]   
         \          |         /       |o|        \          |        /         
        [===Load=Balancer=====]       | |       [===Load=Balancer=====]         
```

---
---
