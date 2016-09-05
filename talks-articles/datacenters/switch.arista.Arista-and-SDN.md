
#### Arista and SDN - Programmable but not Overlay

[techtarget.com/Arista-and-SDN](http://searchsdn.techtarget.com/news/2240161577/Arista-SDN-is-about-programmable-networks-not-overlays)

* Arista provide network programmability via `EOS` (Extensible OS).

* Every process talk with via DB. It's a pub-sub model in EOS.

* Instead of Overlay Controller mode of SDN it allows network to be s/w definable.

* Communications baked in
> * HP/IBM management platforms where network is managed like storage and compute
> * VMWare VSphere/vCloud platforms that map port profile to virtualization
> * OpenStack

* EOS got `Extensible` APIs to talk to any management platform.

* Arista will distribute `OpenFlow Protocols` in switch, then `OpenFlow Controller` can manage multiple switches.

* System database is built on Linux, people can access it and write their own extensions for EOS.

* For programmability, Arista introduced first application switch `FPGA` (Field Programmable Gate-Array).

* (Currently) people using Arista have to dedicate an application and server to get performance they need. Increases hops and thus latency.

* With `FX [application switch]`, can have consistent switching apps right on FPGA and reduce latency.

* Over-subscribed enterprisey `3-tier Network Topology` is gone, condensed to `2-tier`.

* With `VXLAN` one can cross Layer-2 and Layer-3 boundaries and build an extremely large virtual and physical network.

* VXLAN protocol allows to scale from 4K to 16mil VLANs.

---
---
