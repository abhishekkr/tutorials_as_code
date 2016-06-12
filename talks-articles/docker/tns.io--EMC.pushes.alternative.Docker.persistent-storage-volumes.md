## With libStorage, EMC Offers an Alternative to Docker’s Persistent Storage Volumes

> by Scott M. Fulton III, June/2016

[source](http://thenewstack.io/emc-pushes-alternative-dockers-persistent-storage-volumes/)

* [docker launched plug-in system in 2015](http://thenewstack.io/the-real-docker-ecosystem-launches-with-plugins/)

* persistent volumes to solve stateful requirement for some contexts

* [ClusterHQ's dvol](http://thenewstack.io/clusterhqs-new-dvol-start-something-bigger-stateful-apps/), working model for persistent storage via Docker plug-ins

* [ClusterHQ got Docker container to EMC storage](http://www.datacenterknowledge.com/archives/2015/06/18/clusterhq-brings-docker-container-support-to-emc-storage/), enable ClusterHQ's Flocker data volume manager to integrate with EMC devices

* [libstorage](https://github.com/emccode/libstorage), new client library by EMC inside containers that eliminate the need for plug-ins altogether

```
  ,.....................,  ,.....................,
  [Container Runtime    ]  [Container Runtime    ]---,[Operating]
  [ [Libstorage Client] ]  [ [Libstorage Client] ]---'[ System  ]
  '----------\----------'  '-------/-------------'---,
              \-----,       ,-----/                  |
               \     \,---,/     /                   |
                \     |Data|Vol /                    | ,----------,
                 \    '-^-'    /                     | [libstorage]
                  '---, | ,---'                      | [   api    ]
                     .\,|./,                         | '----------'
    ,-------------------------------------------,    |
    |        [ Libstorage Server ]              |----'
    |                  .|,                      |
    |           Storage Platform                |
    '-------------------------------------------'

```

* enable container runtimes to communicate directly with storage platforms
> an architecture that requires plug-in can not, by design, be considered truly `open`
> (libstorage - native storage)[http://pulseblog.emc.com/2016/06/01/news-mesoscon-libstorage-delivers-native-storage-container-platforms/]

* libstorage ain't EMC storage system driver, expects connected volume to be self-maintaining

* best to create a named Data Volume Container, then mount data from it to share data among containers

* CoreOS has been maintaining its wn method to mount attached storage volumes, enabling expanded capacity for container images

* CoreOS launched K8 friendly storage system, [Torus](http://thenewstack.io/coreos-launches-kubernetes-friendly-storage-system-torus/), a distributed pooled block storage system. In future, object storage as well.

* Torus relies on K8 to attach new persistent volumes on an as needed basis by library *flex volume*, acts as a plug-in.

* [Rancher Labs](http://thenewstack.io/tns-makers-aws-reinvent-appcito-sysdig-rancher-runscope/) also created plug-in **Convoy**, enabling storage volumes be created and discontinued directly by an API command from within Container. API also allowed create volume snapshots and storing them within containers.

* [libstorage] also has API for obtaining snapshot of volume.

* EMC trying contest plug-in model stating anything plugged into infra creates dependency.

* EMC has created vendor agnostic interface for connecting runtimes - including Docker containers and Mesos packages - to storage provider services - not only EMCs ScaleIO and XtremeIO, but cloud services such as Amazon's as well.

* Libstorage library will use this interface, REX-Ray to provide double-abstraction. Hiding details of storage volumes from containers and masking container's spec from storage volumes.

---
---
