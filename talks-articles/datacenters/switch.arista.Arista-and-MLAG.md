
## MLAG Overview

[source](https://eos.arista.com/mlag-basic-configuration/)

* `LAG` (Link Aggregation) to bond multiple physical links into combined logical link.

* `MLAG` (Multi-Chassis LAG) extends capacity to allow a downstream `switch` or `host` connect to 2 switches configured as an `MLAG domain`.

* Gives downstream links (switch|host) two uplink paths and full bandwidth as both uplinks appear to be a single switch to `Spanning Tree` (STP), there are no blocked ports.

* Downstream links can use coupled switches as one logical switch for L2 protocols.

* All switches need to run same `EOS` images.

---

### Configure

#### peer link between switch1 and switch2

* ensure control plane ACL is compatible with MLAG, these rule exists in default control plane ACL

```
switch# show ip access-lists
permit tcp any any eq mlag ttl eq 255
permit udp any any eq mlag ttl eq 255
```

> control plane ACL matching on MLAG port and ttl 255 is used to prevent anyone but neighbors on peer link
> show above rules must exist on any custom list as well



#### create port channel for the peer link

assuming eth1 and eth2 connect to two peers

```
switch# config t
switch1(conf)#interface eth1-2
switch1(config-if-Et1-2)# channel-group 10 mode active
switch1(config-if-Et1-2)# interface port-channel 10
switch1(config-if-Po10)# switchport mode trunk
```

#### create a vlan on both switches

* with an unused vlan-id

```
switch1(conf)#vlan 4094
switch1(config-vlan-4094)# trunk group mlagpeer
switch1(config-vlan-4094)# interface port-channel 10
switch1(config-if-Po10)# switchport trunk group mlagpeer
switch1(config-if-Po10)# exit
switch1(conf)#no spanning-tree vlan 4094
```

> trunk group names for peer VLAN should be same on both switches, config must be identical
> this allows safely disabling spanning tree on VLAN4094


#### configure the SVI for peer-to-peer communication

* on switch 1

```
switch1(conf)#int vlan 4094
switch1(config-if-Vl4094)# ip address 10.0.0.1/30
```

* on switch 2

```
switch2(conf)#int vlan 4094
switch2(config-if-Vl4094)# ip address 10.0.0.2/30
```

> test IP connectivity on both


#### configure MLAG peering on both switches

* MLAG configuration for switch1:

```
switch1(config)#mlag
switch1(config-mlag)#local-interface vlan 4094
switch1(config-mlag)#peer-address 10.0.0.2
switch1(config-mlag)#peer-link port-channel 10
switch1(config-mlag)#domain-id mlag1
```

* MLAG configuration for Switch2:

```
switch2(config)#mlag
switch2(config-mlag)#local-interface vlan 4094
switch2(config-mlag)#peer-address 10.0.0.1
switch2(config-mlag)#peer-link port-channel 10
switch2(config-mlag)#domain-id mlag1
```

* running `mlag` on both switches should show status as _active_

---
