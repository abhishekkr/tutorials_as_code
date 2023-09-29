
## Managing Redis with Kubernetes

> Redis Conf 2016
> Kelsey Hightomwer, Google


Challenges
> * self-discovery
> * managing volumes
> * managing recovery

* in an initial redis cluster, ideally 3 masters and 3 slaves

```
  0-5460       5461-10922  10923-16383
   [master]    [master]     [master]
     |           |             |
     |           |             |
   [slave]     [slave]      [slave]
```

Ideal Features
> * Detect Failures
> * Auto-Recovery
> * Easy Scaling


Redis Cluster Deloyment Challenges
> * No support for remapping of IPs/Ports
> * Configuration File Distribution
> * Service Discovery
> * Volume Management


BinPacking on Kubernetes, stack all the services on a single machine unless told otherwise.
Goal is to wait for right peice to show up to distribute.
It increases single node resource utilization. And overall effective utilization.

But to increase availability and reduce large failure, for some services (mainly `data` specific) one plans to spread around services on different machines.

* Pods
> One or more containers and volumes with shared namespaces.
> One IP per pod.

```
 ,--------[10.128.7.4]--,
 |                       |
 | [      redis       ]  |
 |                       |    At runtime disk get mounted.
 |[/etc/redis/redis.conf]--\  Can have configuration also mounted at runtime.
 |  [/var/lib/redis]     | |
 |Pod     '|'            | |
 '-------[GCE]-[Config]--' |
                     '-----'   
```


#### ReplicaSets

> Cluster puts another back if a redis goes bad
> * drive current state towards desired state

get this via Services

##### Services

> Provide a stable endpoint for one or more pods.
> `Service Discovery` baked in by default with `labels`

```
label:
  app: redis
```

```
              [label:redis]10.131.246.27
      _________/ | \____\_______
     /           |      \__     \________
  ,-/-----,  ,---|---,  ,--\----,  ,-----\-,
  | R  P1 |  |P0 | P1|  |   R   |  | P0   R|
  |  P2   |  |   R   |  | P2 P3 |  | P2 P3 |
  '-------'  '-------'  '-------'  '-------'
```

> to be able to idenitfy each as independent in a cluster
> we can map an IPAddress via following mode, where even migrating container will have IP followed

```
label:
  app: redis
  name: redis-1
```

```
    [redis-1] [redis-2] [redis-3] [redis-4]
     /           |        \          \___
  ,-/-----,  ,---|---,  ,--\----,  ,-----\-,
  | R  P1 |  |P0 | P1|  |   R   |  | P0   R|
  |  P2   |  |   R   |  | P2 P3 |  | P2 P3 |
  '-------'  '-------'  '-------'  '-------'
```

> can also a have high-level mapping with it

```
              [label:redis]10.131.246.27
      _________/ | \____\_______
     /           |      \__     \________
  ,-/-----,  ,---|---,  ,--\----,  ,-----\-,
  | R  P1 |  |P0 | P1|  |   R   |  | P0   R|
  | |P2   |  |   R   |  | P2|P3 |  | P2 P3||
  '-\-----'  '---|---'  '---|---'  '------|'
     \           |          |             |
    [redis-1] [redis-2] [redis-3] [redis-4]
```

---
---
