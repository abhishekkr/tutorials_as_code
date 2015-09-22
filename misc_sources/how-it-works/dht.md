# DHT (type: Kademlia)
> It is primarily used to introduce peers to each other in P2P architectures.

### Cogs
> * protocol: to communicate, high level messages
> * topology: how they organize themselve to work
> * routing: how to find right place in cloud of nodes
> * routing table: to make routing table to have the routing capability
> * traversal algorithm: making routing possible

---

### Protocol

4 fundamental RPC messages

* ping
> just syn-ack to check heartbeat

* announce\_peer
> announce address of self

* get\_peers
> who else is on this swarm, get list from known neighbors or reference of who would know
> it's recursive to complete traversal

* find\_node 
> who can tell me about a particular node
> it's recursive to complete traversal

Each message has sender's address. This helps nodes to learn of new nodes from network chatter.

**Some Higher Level**

* bootstrap
> to identify who else is on this DHT, find nodes close to yourself

* refresh buckets
> maintaining list of who is on DHT as of now

* announce
> get yourself added to a peerlist

**Spoof Protection**
> entire protocol is based on UDP, no identity verification
> get\_peers responds with 'write-token'
> write-token is MAC of (source address, target info hash, local expire-on-clock secret)
> announce\_peer requires valid write-token

---

### Topology

Describes how nodes can respond to recursive lookups.
DHT is madeup by all bittorrent peers across all swarms.
Each node has address or nodeID. All nodes uniformly distributed over nodeID space.

* NodeID has same size as info-hash(keys in hash-table): 160bits long

> NodeID is unrelated to your geolocation.

> Centralized Tracker is more stable way of finding peers. You do get IPs and ports here to connect to.

---

### Routing

Nodes whose ID is close to an info-hash are responsible to store info about it.
Instead of knowing about all nodes, a node keeps track of nodes closer to itself.

The routing table orders nodes based on their distance from oneself as per nodeID.
Euclidian distance folds the distance space and nodes are no longer uniformly distributed, a problem
Kademlia uses XOR distance metrics.
``` d(a,b) -> a XOR b ```

The distance is divided into buckets each holding no more than 8 nodes per bucket. Limiting the maximum nodes you care in a bucket.
The space covered by a bucket is half as big as the previous bucket. You are more aware of nodes closer to you.

```
nearest-----------------------------------------------------------------farthest
[5|4| b#3|  bckt#2 |     bucket#1      |               bucket#0                ]
```

Every hop in a recursive lookup , node distance reduces by half.
Lookup complexity O(log n).

---

### Routing Table

XOR distance metric applied to routing table counts the length of common bit-prefix.
The number of bit-prefix shared among nodes, the closer they are.
```
Our Node ID:      1001111101111010101...
Other Node-X ID:  1001100001011011101...
Other Node-Y ID:  0001100001011011101...
5 bit prefixes are common, so other node-x is from bucket#5 which is very near.
No prefix bits are common with other node-y and thus it is from farthest bucket 0.
```

So 160bit bucket space can be cut half 160 times and thus resulting 160buckets max.
Nodes in Bucket#0 will share 0 bit prefixes, bucket#5 will share 5.

```
nearest-----------------------------------------------------------------farthest
[5|4| b#3|  bckt#2 |     bucket#1      |               bucket#0                ]
[5|4|3prx|2prfx'10'| 1prefix '1'       |           no prefix                   ]
```

**View of Routing Table in Node ID space**
When you are in bucket#5, matching first 4,3,2,1,0 prefix bits it will mark other buckets.
```
[              bucket#0                | b#3|5+|4|  bckt#2 |     bucket#1      ]
(first 4 bits 0)                                                (first 4 bits 1)
```

**Formation**
Routing Table starts with just bucket#0.
Addition of 9th node splits up the top bucket, at first which is bucket#0.
Nodes are shuffled to their respective buckets.
After top bucket reaches counter 1, whatever nodes are received for lower buckets have to be rejected.
As the max nodes per bucket need to be 8.
And all the nodes not belonging to lower bucket have already been shuffled to top bucket.

> Here, you can prioritize low-latency peers to optimize on speed.

---

### Traversal Algorithm

**Steps**
* Pick known nodes from routing table which are close to desired target into a 'list'.
* Sort them by their XOR distance to target
* Send requests to 3(or manageable count) at a time, these request will fetch more closer nodes to target. As our selected nodes will have have their node list to refer.
* Received nodes are maintained in same sorted 'list', we ignore the nodes already known. Also the nodes not respoding are marked stale, not removed.
* For next set of request again the most near X(3) nodes are selected and queried. This continues some(10-20) times.

Termination Condition is the top (nearest) 8 nodes of the 'list' are all queried and they responded. Determining those are nearest to target available to our node.

Then our node 'announce_peer' to these top 8 nodes to get added to their peer list.

---
---
