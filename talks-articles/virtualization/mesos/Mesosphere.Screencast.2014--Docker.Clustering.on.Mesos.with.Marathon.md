
## Docker Clustering on Mesos with Marathon
> Mesosphere Youtube Channel

> inspired from a section at `Docker Global Hackday 2014`
> [libcluster](https://youtube.com/watch?v=vtnSL79rZ6o) PoC, Cluster by Andrea Luzzardi and Victor Vieux

```
    [Hadoop Cluster]  [MPI Scheduler]       ()-() Zookeeper Quorum
           \         /,----------------------'()'
            \       //                 ,------'|
      ,-[Mesos Master]  [Standby Master]  [Standby Master]
     /       '-------,'-----------------------,
   ,................, \  ,................, ,................,
  [Mesos Slave      ]  [Mesos Slave      ]  [Mesos Slave      ]
  |________________ |  | _______________ |  | ______  _______ |
  |[Hadoop Executor]|  |[MPI    Executor]|  |[Hadoop||MPI Exe]|
  ||  [task]]]     ||  ||  [task]]]     ||  ||[tasks||[tasks]||
  |'---------------'|  |'---------------'|  |'------''-------'|
  '-----------------'  '-----------------'  '-----------------'

```

* `:5050` mesos
* `:8080` marathon

* `ls /etc/mesos-slave`
> dirs: attributes
> fles: containerizers, executor_registration_timeout, hostname, ip

* `cat /etc/mesos-slave/containerizers`
> docker,mesos

* `cat /etc/mesos-slave/attributes/host`
> `hostname...`
> this attributes section is used to provide metadata about slave having certain features(devices,)

* config of mesos cli @ `~/.mesos.json`, it points to master

* `mesos ps` to see what tasks are currently running

* launch an app
```launch.sh
[[ $# -ne 1 ]] && exit 1
curl -X POST -H "Content-Type: application/json" $MESOS_MASTER:8080/v2/apps -d@"$@"
```

```simple-app.json
{
  "container": {
    "type": "DOCKER",
    "docker": {
      "image": "libmesos/ubuntu"
    }
  },
  "id": "ubuntu",
  "instances": "2",
  "cpus": "0.25",
  "memory": "256",
  "uris": [],
  "cmd": "while sleep 10; do date -u %T; done"
}
```

```
./launch.sh simple-app.json

mesos ps
```

```simple-app.json
{
  "container": {
    "type": "DOCKER",
    "docker": {
      "image": "libmesos/ubuntu"
    }
  },
  "id": "ubuntu",
  "instances": "2",
  "cpus": "0.25",
  "memory": "256",
  "uris": [],
  "cmd": "while sleep 10; do date -u %T; done"
}
```

---

## Handling conflicts of network

* default network model is `host` mode, can cause conflict

```redis-app.json
{
  "container": { "type": "DOCKER", "docker": { "image": "redis" } },
  "id": "redis"
  "instances": "2",
  "cpus": "0.25",
  "memory": "256",
  "uris": []
}
```

* can make one redis run per slave, a way to handle such conflicts

```redis-unique-app.json
{
  "container": { "type": "DOCKER", "docker": { "image": "redis" } },
  "id": "redis"
  "instances": "2",
  "cpus": "0.25",
  "memory": "256",
  "uris": [],
  "constraints": [["hostname", "UNIQUE"]]
}
```

* check on processes just started:w

```
mesos ps

mesos tail redis
```

* switching network set-up to `bridge` mode, handling conflict, on nodes idenitfied as `rack-2`

```redis-bridge-app.json
{
  "container": {
    "type": "DOCKER",
    "docker": { "image": "redis", "network": "BRIDGE" }
  },
  "id": "redis"
  "instances": "2",
  "cpus": "0.25",
  "memory": "256",
  "uris": [],
  "constraints": [["rack", "CLUSTER", "rack-2"]]
}
```

* nginx running in Bridged mode with port-mapping provided by mesos

```nginx-bridge-port-app.json
{
  "container": {
    "type": "DOCKER",
    "docker": {
      "image": "nginx",
      "network": "BRIDGE",
      "portMappings": [
        { "containerPort": 80, "hostPort": 0, "servicePort": 80, "protocol": "tcp" }
      ]
    }
  },
  "id": "nginx"
  "instances": "2",
  "cpus": "0.25",
  "memory": "256",
  "uris": []
}
```

---

* HAProxy load-balancing scaling up web-services with dynamic configuration

---

#### Q & A

* ZK used for Master Discovery and HA mode, for leadership election

* ZK used for scheduler state, (Apache Mesos) also held in replication logs

* CustomTags in Mesos ecosystem are referred to as `attributes`

* Replaceable Scheduler
> Mesos is a 2-level scheduler, Meso provides offers
> Scheduling is done by framework, like Marathon (replaceable)

* Relationship of containers, grouping exists

* `Apache Mesos` project have 2 examples of writing custom scheduler `mesosphere/Rendler`

---
