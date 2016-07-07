## Juniper Networks' Trivia on  DataCenter
> Youtube Channel, multiple videos
> Doug Hanks, Steve Levine

### DataCenter Bridging?

* DCB is suite of protocols to allow converge storage in DataCenter.

* Collapse storage and data into single network. Require lossles transfer protocols.

---

### Priority-Based Flow Control in DataCenter?

* PFC is a protocol for DCB. If server talks to storage devices, they can tell each other to slow down in case resource contention.

---

### Preserve lossless behavior on SDN or overlay network?

* SDN or overlay makes your network config act at Layer-3

* Can use PFC. Can see how point to point link are cnfigure to enable PFC.

---

### Overlay network in a DataCenter?

* SDN's overlay provides agility to maintain network programmatically.

* De-couples the logical network from physical hardware.

---

### Virtual Chassis Fabric?

* With Overlay Network, acquire IP Fabric. It encapsulated is Virtual Chassis Fabric.

* Single point of management.

* It's plug-n-play ethernet fabric.

---

### How does ISSU work on QFX 5100?

* Virtualized control plane with KVM, JunOS running inside it.

* When need ISSU, start a new instance of ISSU and then switch control.

* No loss in Data Plane.

---

### What is best Control Plane protocol for DataCenter IP Fabric?

* For IP Fabric
> advertise prefixes
> move from point.A to point.B in network
> traffic tagging (keep track of what came from where)

* Options
> OSPF
> ISIS
> BGP

* BGP
> can do traffic tagging and engineering
> best use-case Internet, runs on BGP

**but keep tabs on security flaws in implementation**

---

#### What is DCBX and why use it in DataCenter?

* DataCenter Bridging eXtensions.
* Collapse data and storage on same hardware. Enable plug-n-play.
* It enables switches automatically identify what capability is there.

---
---
