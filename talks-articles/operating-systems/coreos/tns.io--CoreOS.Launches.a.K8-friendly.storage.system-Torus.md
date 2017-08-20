
## CoreOS launches a Kubernetes-friendly storage system - Torus

[source](http://thenewstack.io/coreos-launches-kubernetes-friendly-storage-system-torus/)
> Joab Jackson

[Torus](https://github.com/coreos/torus)

* it's an opensource project aimed to give Kubernetes users a proper scalable storage system to work with their pods

* runs within a container, provide single distributed storage cluster to manage  microservices workloads

* for Torus, K8 keeps track of all different resources within the cluster

```
            Kubernetes Pods
    ,----------,   ,----------,  ,----------,
    | [P] Pod  |   | [P] Pod  |  | [P] Pod  |
    '----------'   '----------'  '----------'
         |              |             |
      [=Mount=]      [=Mount=]     [=Mount=]
         |              |             |
   ,--------------------------------------------,
   |            Torus Storage Pool              |---<> etcd
   '--------------------------------------------'
      |   |   |   |   |   |   |   |   |   |   |
     [@] [@] [@] [@] [@] [@] [@] [@] [@] [@] [@]
                     Node Disks
```

* There is a whole set for running distributed storage systems, like GlusterFS and Ceph. But they can be tricky to use. They were not designed for large-scale containers but small cluster of large machines.

* CoreOS built Torus following [Google GIFEE- Google Infra for Everyone Else](https://www.youtube.com/watch?v=juCQMnSfysQ)

* Torus written in `Golang`, uses `gRPC` protocol; easy to write clients

* Torus manages all disks under as a single storage pool, can scale to 100s of nodes easily.

* K8 pods get it as traditional filesystem

* K8 allows deploy Torus via K8 manifests, allowing admins to run Torus as [K8 managed app](http://thenewstack.io/openstack-gets-self-healing-coreoss-new-kubernetes-based-stackanetes/)

* Torus supporting block-oriented storage via NBD (Network Block Device)

* Can be encrypted. Provides hashing, replication, GC and pool re-balancing

* Torus computes data placement across all nodes across cluster automatically

* **At its core, Torus is a library with an interface that appears as a traditional file**

* Coordinated and check-pointed through `etcd's` consensus process

---

#### More organizations supporting

* Docker partnered with [Hedvig](http://www.hedviginc.com/blog/how-hedvig-software-defined-storage-integrates-with-docker-datacenter) and [BlockBridge](http://www.blockbridge.com/docker/)

* EMC offers [REX-Ray](http://rexray.readthedocs.io/en/stable/), designed to provide persistent storage access for Docker and Mesos-based container

* [Kubefuse](https://opencredo.com/introducing-kubefuse-file-system-kubernetes/), a K8 friendly FS that allows admin to carry out handy tasks

---
---
