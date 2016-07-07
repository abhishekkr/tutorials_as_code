## DataCenter Network Topologies
> by Raj Jain
> Scholartica (2013 or so)

### Part.1

details around DataCenter solutions available, physical layout


### Part.2

#### Network Topologies

* Servers connected to `Access Switches`
* `Access Switches` connected to (at least) 2 `Aggregation Switches`
* `Aggregation Switches` connected to (at least) 2 `Core Switches`o
* `Aggregation` layer is transition point between L2-switched access layer and l3-routed core layer

> The topology would have difference on practitioner.
> It might be `Access Switch` -to- `Aggregation Switch` -to- `Access Router` -to- `Core Router`, (as in a document from Microsoft).
> Where `Core Routers` manage traffice between `Access Routers` and `in/out DataCenter`.
> All switches below `Access Routers` forming single `Layer 2` domain.


```cisco
          {RemoteUser}----,    {User}   ,-----{RemoteUser}
                           \     |     /
                           [WAN/Internet]
                             |      |
                           (===)  (===)  <-Edge Routers
                            /\      /\
                         ,-/--+----' .\--------------------------,
          Campus Core  []-[]  '-------+[]-[] DataCenter Core     |
                       | X |         |  |X|                      |
      Campus Distribu- [] []         | [] [] DataCenter          |
           -tion       | X |         | | X | Aggregation         |
                       |/ \|         | |/ \|                     | everything is dual connected
      Campus Access   <=> <=>        | <=> <=> DataCenter Access |
                       |   |         | / \ X \                   |
      Campus User      {} {}         |{} {} {}{} Server          |
                                     '---------------------------'
```

---

#### Hierarchical Network Design

* all servers require application delivery service for security (VPN, IDS, Firewall), performance (Load Balancer), networking (DNS, DHCP, NTP, FTP, RADIUS), DB services (SQL)
* ADCs (Application Delivery Controllers) are located between the `Aggregation Routers` and `Core Routers`.
* Stateful devices (firewalls) on `Aggregation` layer

---

#### Access Aggregation Connections

* Looped Triangle
> Most common. Spanning Tree Protocol (STP) blocks links. Hardware might go unused.

* Looped Square
> Oversubscription doubles on failure.

* Loop-Free-U
> No L2 communication between aggregation switches if any switch links fail.

* Loop-Free-Inverted U
> Black-holes on some failures.

---

### Part.3 Issues

* Oversubscription, higher layer oversubscribed. Also, can't move servers easily.

* Moving across `Subnets` is painful
> Ethernet address remains fixed. IP address change when moved rack, networks.
> Requires to re-configure IP addresses and VLAN trunks.

* Service trample on each-other, overuse by one leaves less for others.

* Poor reliability. one access switch failure doubles the load on the other.

* Under-utilization.

* ECMP (Equal Cost Multipath) used by routers to spread traffic to next hops using a hash function. Only few paths.

---

### Part.4 Requirements

* VLANs to wall different user groups from each other

* need to be Scalable, Secure, Shared, Standardized, and Simplified (5S's)

* converge infrastructure - server, storage and network

* L2 domains need VM mobility

* congestion management on Ethernet


#### Facebook's `4-Post Architecture`

* each Rack Switch (RSW) has upto 48 10G downlinks and 48 10G uplinks (10:1 Oversubscription) to cluster switch (CSW)

* each CSW got 4 40G uplinks - one to each of 4 FatCat (FC) `Aggregation Switches` (4:1 Oversubscription)

* 4 CSW's are connected in 8x 10G protection ring, 4FC's are connected in a 16x 10G protection ring.

* No routers at FC. One CSW failure reduces intra-cluster capacity to 75%.


#### Clos Networks

> one of the current research topic, [details](http://en.wikipedia.org/wiki/Clos_network)

* Multi-stage circuit switching network proposed by Charles Clos in 1953 for Telecom Switching Systems

* allows to form a large switch from smaller switches, reduces number of cross-points, lower cost

* strict sense of non-blocking

> makes 'Fat-Tree' topology happen, improves performance and reliability

---

> DataCenter Ethernet

### Part.5 DataCenter Bridging Extensions

Four standards (mainly for storage traffic)

* Priority-based Flow Control (PFC)
> In 'Flow Control'
> Pause Frame. Stops sender from sending any more packets for time specified in Pause Frame.
> Causes Congestion Backup. Frame is multicase, not forwarded.
>
> In 'PFC', any single priority can be paused. Higher priority can continue, lower can be backed-up or dropped.


* Enhanced Transmission Selection
> guarantee b/w of application sharing a link
> traffic divided into 8 classes (not priorities), classes then grouped
> classes don't have order like priorities
> fairness (balancing) maintained within a group, b/w allocated per class group
> unused b/w gets used by others

* Congestion Control
> QCN (Quantized Congestion Notification) sends a source quench message by congested switch direct to source.
> Source reduces its rate for that flow. Easy for switch complex for hosts.
> Source maybe a router in a subnet and not real source. Route will drop traffic, QCN doesn't help in this case.


* Data Center Bridging Exchange
> DCBS uses LLDP to negotiate qualit metrics and capabilities for PFC, ETS and QCN with Neighbors
> LLDP - Link Layer Discovery Protocol

Don't wanna go to TCP or even IP, due to unreliability for packets. Need high layer management.

---
---
