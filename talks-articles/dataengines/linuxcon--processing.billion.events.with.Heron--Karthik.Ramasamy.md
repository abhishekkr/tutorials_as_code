

## Processing Billions of Events in Real-Time with Heron
> by Karthik Ramasamy
> LinuxCon North America, ContainerCon

#### Stream Grouping

* Shuffle Grouping : random distribution of tuple to downstream bolts

* Fields Grouping : group tuple based on hash of one/multiple fields

* All Groupings : replicate tuples to all tasks

* Golbal Groupings : send entire stream to one task

> combination of shuffle and fields grouping more common

---

### Why Heron?

* Performance Predictability

> in `Storm`, no clear control on how much resources a topology to use, preventing one topology eat other's resource


* DeveloperProductivity

> easier for teams to develop and debug toplogies for bugs and performance


* ease of manageability

> `Storm` forced to manage individual cluster for topologies

---

### Heron Design Decisions

* fully API compatible with `Storm`

* task isolation/containers

* support of mainstream languages like C++/Java/Python

---

### Heron Architecture

* Got away from custom scheduler as before, Mesos/Aurora are well supported and have community behind them. Streaming jobs run as service until killed.

```

      [Scheduler]------------------>[topology 1]
      /'     \  \________________
     /        \                  '->[topology 2]
  {Toppology}  '-----------------        :
  {Submission}                   '->[topology n]

```

* Whenever you launch a topology, Containe-0 comes up and is called Toppology-Master. It takes care of marking host/port metadata into Zookeeper cluster so all fragments of that Toppology can find each other.

```

            [Topology]------------------------>[ZK     ] logical plan, physical
           ,[ Master ]    ,--------------------[Cluster] plan and execution plan
           /        \___ /_______________________    |
          /    _________/sync physical plan      \   |
     ,--,/----/-------------,                    ,\,-|,-----------------,
     | [Stream ]  [Metrics] |                    | [Stream ]  [Metrics] |
     | [Manager]  [Manager] |                    | [Manager]  [Manager] |
     |                      |                    |                      |
     | [l1] [l2] [l3] [l4]  |                    | [l1] [l2] [l3] [l4]  |
     '----------------------'                    '----------------------'
         container                                     container

```  

* Topology-Master determines resources required, communicates with scheduler and spawn data-containers.

* `Stream-Manager` comes up on data-containers and uses ZK to identify Topology-Master.
> Once all `Stream-Manager` check-in with Topology-Master, it prepares a Physical Plan in ZK.
> It allows all `Stream-Manager` to reach each other.

* All containers have UUID, so if a data-container or Topology-Master dies and re-spawned. Physical Plan gets updated and discoverability doesn't break.

* Actual tasks (bolts and spouts) get executed in other process in these containers marked as `l1,l2,l3,l4`. Metrics-Manager keeps track of overall performance.

---

### Why Is That There

#### Topology Execution

* every container got a control socket and a data socket

* even local exchange of data, it bounces off `Stream-Manager` to keep architecture simpler


#### Topology-Master

* assigns role, whose responsible for what

* monitors health

* keeps track of metrics


#### Stream-Manager

* routes tuple based on grouping to appropriate container and process within

* back pressure, to manage slower bolts

* ack management


#### Heron Instance

* does the real work, is atomic

* exposes API

* provides metrics

```
                             ,----------------------------------------------,
                             |              data-in queue   [''''''''''']   |
  [Stream-Manager]<-------,  |     ,-----,----------------->[ Task      ]   |
                           '-|--->[Gateway]<----------------[ Execution ]   |
                           ,-|--->[ Thread] data-out queue  [ Thread    ]   |
  [Metrics-Manager]<------'  |     '------'<----------------[___________]   |
                             |              metrics-out queue               |
                             '----------------------------------------------'
```

---

### Heron Deployment

```
          [ Aurora  ]      [   ZK  ]       ,-------->[Topology 1]
          [Scheduler]      [Cluster]      /
                              |'         /     ,---->[Topology 2]
       aurora services        |         /     /           :
      ,-----------------------+--------/---, /            :
      | [Heron]               |       /    |/             :
      | [ Web ]               |      /     /              :
      | /'                  [Heron  ]-----'|              :
 User-+<                    [Tracker]--,   |              :
      | \                               \--+-----,        :
      |  \,                                |      \->[Topology N]
      | [Heron]                            |
      | [ VIZ ]----------------------------+-->[observability]
      '------------------------------------'

```

---

### Heron @Twitter

been in Prod for 2years till this talk, running in shared mode on few 1000 nodes cluster onver Mesos/Aurora.

* Use Cases
> * Real-Time ETL
> * Real-Time BI
> * Product Safety
> * Real-Time Trends
> * Real-Time ML
> * Real-Time Media Operations
> * Real-Time Ops (Predictive)

---

### Componentizing Heron

* Unmanaged Schedulers
> YARN
> Mesos

* Managed Schedulers
> Aurora
> Kubernetes
> Amazon ECS
> Marathon

* State Managers
> Zookeeper
> Hadoop
> Local FS


* Job Uploaders
> Hadoop
> Amazon S3
> Local FS

---

### Micro Streaming

**so even if tools in ecosystem switch, the underlying/glue design doesn't have to**

* basic inter/intra IPC, for inter process communication
> for Topology-Master, Stream-Manager, Instance, Metrics-Manager

* scheduler SPI, scheduler to fit in
> for Topology-Master

* state manager SPI
> for Topology-Master

* metric manager standard
> for Graphite, Scribe


#### Features

* plug-n-play components
> environment changes, core doesn't

* multi-language instances
> multiple language API with native instances

* multiple processing semantics
> efficient stream managers for each semantic

* ease of development
> faster development with little dependency

---

### Heron Environment @Twitter

* Local
> Local Scheduler, Local State Manager, Local Uploader

* Testing
> Mesos/Aurora Scheduler, ZK State Manager, HDFS Uploader

* Production
> Aurora Scheduler, ZK State Manager, Packer Uploader

---

### Stragglers and BackPressure

> Stragglers occur in multi-tenant distributed systems due to
> * Bad Host
> * Execution Skew
> * Inadequate Provisioning


Approaches to handle Straggler

* Senders to Drop
> was used in Storm
>
> unpredictable, affects accurace, poor visibility

* Sender to Slow Sending Rate (BackPressure)
> is used in Heron, sockets are removed fromm Spouts to give bolts some time
>
> predictable, processes data at maximum possible rate, reduce recovery times, handles temporary spikes
>
> in most scenarios backpressure resolves issue without manual intervention
> in few due to irrecoverable GC cycles or faulty hosts, it doesn't and requires manual restart
>
> can run another topology to blacklist containers based on performance

* Detect Stragglers and Reschedule them

---

### Active Projects

* auto-scaling of topologies
* root-case detection
* open vs closed streaming systems
* sampling vs dropping data

---
---
