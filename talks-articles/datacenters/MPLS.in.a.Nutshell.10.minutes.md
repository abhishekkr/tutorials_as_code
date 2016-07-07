
## MPLS in a Nutshell (under 10 minutes)
> Keith Barker (Xine)

```

              \    /                   \     /
          {^^^^\^^/^^^^^^^^^^^^^^^^^^^^^\^^^/^^^^}
         {     (PE1)                   (PE4)     }
      \ {     _/                            \_     }
       \     /                                \     }
      { '--(PE2)------------(P)--------------(PE3)   }
       {   /  \                              /  \   }
        ~~/~~~~\~~~~~~~~~~~~~~~~~~~~~~~~~~~~/~~~~\~~
         /      \                          /      \
       (CE-A2)  (CE-B1)                  (CE-A1)  (CE-B2)
        remote   HQ                        HQ      remote

acronyms
* Providers: P,PE1,PE2,PE3,PE4
* Customer Nodes: CE-A1,CE-A2,CE-B1,CE-B2
```

Layer3 VPN solution using MPLS solution.

Customer Alice got several sites, say site EAST-A and EAST-B.

In past create Virtual Circuits (using `Frame Relay Circuit`) was setup between different machine between different sites for communication.
Then do overlays (doesn't scale well) to multiple sites. Used with ATMs.

---

Now this can implemented at good scale with MPLS Layer3 VPNs.

Say Customer Alice got `10.0.0.0/8` network at Headquarters East-A machine `CE-A1`. `CE-A1` set up a peer with provider edge `PE3`.
Peering can be `OSPF`, `EIGRP`, `RIP` or `BGP`.
PE3 will sign this interface to `VRF` (Virtual Routing Forwarding Table, like a VMWare image of routing table)

VRF is a separate Rotuing Tables. These rules get transported to rest of provider network over `Multiple Protocol BGP`.
`PE2` will get these routes and exchange it with `CE-A2`, on its `VRF` with it. `PE2` to `CE-A2` can have another routing protocol than `PE3` to `CE-A1`.

Now `CE-A2` has learned of routing to `10.0.0.0/8` network of `CE-A1` in this mechanism.

MPLS is only used in Service Provider Network to label top switches.
The VRF sent by PE3 is like a VPN label, so anytime a packet marked for that VPN label is sent MPLS makes sure to make it reach the `porivder` who published that VRF.


---
