## Intro to OpenFlow and SDN
by Dan Pitt, OpenNetworkingFoundation

### Generational Shift

> * FROM: Static, inflexible, networks, not helpful for new business initiatives, little agility and flexibility.
> * TO: Programmable networks that enable new business initiatives through flexibility, agility and virtualization.

> * From 'Hardware/Appliances' to '(Open) Software'

> * From 'Distributed Control Panel' to 'Logically Centralized Control Plane'

> * From 'Custom ASICs/FPGAs' to 'Merchant Silicon'

> * From 'Protocols' to 'APIs'

---

### SDN Abstractions

* FORWARDING: Common API for programming network hardware

* STATE DISTRIBUTION: Single state-distribution algorithm for a network

* GLOBAL MANAGEMENT: Programmer interacts with entire network instead of individual nodes

---

### What SDN looks like

```
  [App] [App] [App] [App] [App]   ]  Application Tier

  ----------------------------    \
 [ SDN Control Plane Software ]    | Control Panel Tier
  ----------------------------    /
    |    |         |       |      \
    |    | [Switch]C     [Switch]  \
  [Switch]         |                | Data Plane Tier
         |       [vSwitch]         /
      [vSwitch]                   /
```

* *Application Tier*: Virtual network overlays; network slicing (delegation); tenant-aware broadcast; app-aware push computation; intregation with other Software packages, policy, traffic engineering

* *Control Panel Tier*: Data plane resource marshaling, common libraries (e.g. topology, host metadata, state abstraction)

* *Data Plane Tier*: Packet forwarding and manipulation (as per flow table), statistics collection

---
---
