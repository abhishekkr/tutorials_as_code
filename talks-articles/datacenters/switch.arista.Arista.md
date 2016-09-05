
## Arista

> Trivia

#### Quick Tips

* `traceroute` to check all leaf nodes are available from each other

* `show ip mroute` to confirm multicast is not configured

* Base is linux, all device specific code is userprocess.

* provide a json `eAPI` ecosystem to use (inMon, Spotify's pyEOS, more)

* `Ansible`, `Chef`, `Puppet`, `Splunk` and `RedHat` are ecosystem partners

* 4 types of programmability

```
 [XML   /  JSON           |]<----- High Level API
 [CLI   /  SNMP           |]
 [ Protocols              |]
 [ Protocol Infrastructure|]
 [ Platform Code          |]
 [ HAL                    |]<----- H/w API
 [ Kernel                 |]<----- Linux API
 [ Hardware               |]

4th approach is DB approach with EOS' SysDB

```

* Arista EOS architecture

```
    -------------------------------------
    [      Arista EOS Architecture      ]
    -------------------------------------
                  {PIM}
      {SNMP}<-,    '|'         ,>{BGP}
              --,-------------'
     {Driver}<- {  SysDB  }<-----{IGMP}
             ---^   '|'   ^--
      {STP}--'       |       '--{eAPI}
                  {MLAG}
    -------------------------------------
    [   Efficient Publish/Subscribe     ]
    -------------------------------------
    [         Linear Cloud Scale        ]
    -------------------------------------
```

* `CloudVision` is like Network-wide SysDB, aggregation of network-wide SysDB. Provides single integration point to network. Platform for automation and visibility across network.

---

#### upgrade

* Smart System Upgrade (SSU)
> automated rolling upgrades for Spine|Leaf
> so minmal outage; `reload`, `reload fast-boot`; now fastest `reload hitless` taking 17ms

---
---

sources

* [Arista Network Overview](https://www.youtube.com/watch?v=dcrrQCzzWUs)

---
