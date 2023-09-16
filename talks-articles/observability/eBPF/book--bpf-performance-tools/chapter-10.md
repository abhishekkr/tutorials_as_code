
## Chapter.10 Networking

> Cillium project for K8s based networking & security. BPF allowing tracing at multiple layers as app, net libs, syscalls, TCP/UDP, IP & NIC device drivers.

### Background

* Network Stack

```
                  ,------------------------------------------------,
 [App]--[Lib]--[SysCalls]--send/recv--[VFS]                        |
                  |   |                 |                          |
                  |   |,connect/accept  |,                         |
                  | [Socket (send/recv buffers)]                   |
                  |   |           |       '------>[ICMP]-,---,     |
                  |   |           '-------------->[UDP ]-| IP|-,   |
                  |   '-------------------------->[TCP ]-'---' |   |
                  |                                            |/  |
                  |                  [Queueing Discipline (qdisc)] |
                  '-----------------------|-------|--------|-------'
                         Device Drivers [ena] [ixgbe]   [...driver queues]
                                          |       |        |
                                        [NIC]  [NIC]  [NIC/VirtDevices]

 * These paths include kernel bypass and BPF-XDP.
```

* App can bypass kernel n/w stack using DPDK (Data Plane Development Kit) for higher packet rates & perf. App implements n/w protocol in user-space & write to n/w driver via DPDK & Kernel userspace I/O (UIO; or Virtual Fn I/O i.e. VFIO) driver. Traditional metrics isn't available here.

* XDP (eXpress Data Path) provides a fast path for packets via eBPF. It's integrated into Kernel stack by accessing n/w ethernet frame in NIC driver via BPF-hook. Can do quciker accept or drop decisions; allowing fast DDoS mitigation & s/w defined routing.

* Packets pass via Kernel using `sk_buff` socket buffer struct. Socket struct as `tcp_sock`. Protocol attached with struct proto as `tcp_prot`, `udp_prot` defining callback fn for `connect`, `sendmsg` & `recvmsg`.

* Policies are available to distribute NIC interrupts across CPUs including API (NAPI) interface, Receive Side Scaling (RSS), Receive Packet Steering (RPS), Receive Flow Steering (RFS), Accelerated RFS, Transmit Packet Steering (XPS).

* `SO_REUSEPORT` option allows a pool of processes or threads to bind to same socket address; Kerne; balances new connections across pool of bound threads. BPF program can steer via `SO_ATTACH_REUSEPORT_EBPF`.

* To prevent Slowloris like attacks; linux uses 2 queues: a SYN backlog with minimal metadata to survive SYN flood (by simple dropping) and a listen backlog for completed connections.

* Timer-based TCP retransmit of failed packets based on `TCP_RTO_MIN` cause further delays. Fast retransmits if duplicate ACKs arrive. Selective ACK (SACK) TCP option use avoid re-sending all packets sent after single failed.

* Linux dynamically alloc send/recv buffer and allows tuning. Larger size improve perf at more mem per conn cost. Better NIC support TCP Segmentation Offload for super packets, improving n/w stack throughput.

* TCP congestion control algo (like Cubic, Reno, Tahoe, DCTCP & BBR) modify send/recv windows.

* Optional layer of Queueing Discipline manages traffic classification (tc), scheduling. manipulation, filtering & shaping of packets. List of options via `man -k tc-`.

* Several algo in use at n/w stack can be looked at

> * `Nagle`, reduces small n/w packets by waiting & coalescing.
> * `BQL` (Byte Queue Limits) manage driver queue size to avoid starvation & reduce latency.
> * `Pacing` to control send avoiding bursts.
> * `TSQ` (TCP Small Queues) manages bufferbloat.
> * `EDT` (Early Departure Time) orders packet sent to NICusing timing wheel instead of queue.

* Various latency metrics for insight

> * `Name Resolution Latency` for hostname to IP. `Ping Latency` from ICMP echo request.
> * `TCP Connection Latency` between sending SYN to receiving ACK. `TFO` (TCP Fast Open) helps.
> * `TCP first-byte Latency` (i.e. time to first byte `TTFB` latency), for receiving first data byte on a new connection.
> * `RTT` (round-trip time) of a packet. `Connection Life Span` from open to close; also if keep-alive is in use.

> * Socket I/O details. Process creating TCP sessions. Errors are at socket, TCP or IP-level.
> * TCP window sizes and if any zero-size transmits. I/O size at different stack layers.
> * Dropped packet details. TCP latencies. Kernel inter-stack latencies. Latency in queues.
> * Protocols in use.

* BPF only adds a tiny overhead to each event; but millions of event would quantify it to noticeable. Thus sample & trace specific events for related issues with lower frequency. E.g. tracing TCP retransmits with `tcp_retransmit_skb` only without tracing each packet is efficient.

* Sample Strategy:

> * Get simple stats as packet rates & throughput (`ss`, `nstat`, `netstat`, `sar`, `nicstat`).
> * Trace new connections, their durations (`tcplife`). Might have unnecessary frequent connection that can be cached.
> * Unusual TCP events as retransmits (`tcpretrans`, `tcpdrop`, tracepoint `skb:kfree_skb`).
> * Hostname resolution latency (`gethostlatency`). Network latency at different points (compared to idle time).
> * Examine n/w events against known workload (`iperf`, `netperf`).

* Common tracing mistakes:

> * Events may not happen in app context. Selecting different events from app context may help.
> * Using known workload ensure packet and byte counts match.
> * TCP has full & non-full (request socket before complete handshake) sockets. Some socket struct fields may not be valid for non-full.


### Traditional Tools

* `ss` (socket stat); `-t` for tcp only, `-i` for internal info & filter on it, `-e` extended info, `-p` proc info, `-m` mem info. `-s` for summary.

* `ip` (ip stat) can manage routing, dev, interface & tunnels. Shows errors, drops, overruns, collisions.

* `nstat` (n/w stack stat) prints n/w metrics with SNMP names. `-s` avoids resetting counters. Also has daemon mode.

* `netstat` lists open sockets by default. Switch: `-a` all sockets, `-s` stack stats, `-i` nic stats, `-r` routes.

* `sar` record system stats as a monitoring tool. `-n DEV` nic stats, `-n EDEV` nic error stats. Similarly usable option values are `IP, EIP, IP6, EIP6, ICMP, EICMP, ICMP6, EICMP6, TCP, ETCP, SOCK, SOCK6`.

* `nicstat` (n/w interface stat)

* `ethtool` (query or control n/w driver); config with `-i $DEV` & `-k $DEV` and driver stats with `-S $DEV`.

* `tcpdump` (packet analysis)

```
ss -l  ## display all listening sockets; -x for unix sockets only
ss -at  ## display all tcp sockets; -au for udp; -ltu for listening tcp & udp
ss -Kau  ## to kill all UDP sockets
ss -o state established '( dport = :ssh or sport = :ssh )'  ## all ssh connections
ss -o state connected  ## all in connected state

ip addr
ip route
ip neigh

netstat -tunlap

sar -n DEV,SOCK,TCP,ETCP

 tcpdump port 80 or port 8080
```


### BPF Tools

```
                    ,--------------------------,
                    | App                      |    (tools from BCC & bpftrace)
                    |--------------------------|
                    | SysCall Interface        |<--sockstat, socfamily, connect,
                    |--------------------------|   soaccept, soconnlat, so1stbyte
                    |  Sockets                 |<--soprotocol, socketio, socksize,
tcpsynbl, tcpwin,   |--------------------------|   sockrmem
tcpconnect, tcptop->|   TCP    |    UDP        |<--udpconnect
tcpaccept, tcplife, |--------------------------|
tcpretrans, tcpnagle|         IP               |<--superping, ipecn, gethostlatency
                    |--------------------------|
                    |   Queueing Discipline    |<--qdisc-fq, qdisc-cbq,
                    |--------------------------|   qdisc-fq_codel,..
                    |     Network Device       |<--nettxlat, netsize, skbdrop
                    |--------------------------|   skblife
                    |       Link Layer         |<--ieee8021scan
                    |----------:---------------|
                    | Device Drivers           |<--bpftrace
                    '--------------------------'
```

* `sockstat` counts for socket related calls; code similar to

```
#!bpftrace

BEGIN {
    printf("Tracing sock statistics. Output every 1 second.\n");
}

tracepoint:syscalls:sys_enter_accept*, tracepoint:syscalls:sys_enter_connect,
tracepoint:syscalls:sys_enter_bind, tracepoint:syscalls:sys_enter_socket*,
kprobe:sock_recvmsg, kprobe:sock_sendmsg {
    @[probe] = count();
}

interval:s:1 {
    time();
    print(@);
    clear(@);
}
```

* `sofamily` tracing new socket alongwith proc & family name.

* `tcpsynbl` tracing TCP SYN backlog limit & size. `tcpwin` tracing TCP send congestion window size & other params.

* `tcpnagle` tracing usage of TCP nagle on TCP transmit codepath, measure duration of transmit delays.

* `ipecn` trace IPv4 inbound explicit congestion notification (ECN) events. Code similar to

* `superping` measures ICMP echo req to response latency.

* `solisten` list socket listen calls. `tcpstates` list TCP session state changes. `tcpdrop` for packet drops.

* `sofdsnoop` trace file descriptors via Unix sockets.


### BPF One-liners

* Count failed socket connects via `argdist -C 't:syscalls:sys_exit_connect():int:args->ret:args->ret<0'`.

* Count socket connect `stackcount -U t:syscalls:sys_enter_connect`.

* Send bytes `argdist -H 'p::tcp_sendmsg(void *sk, void *msg, int size):int:size'`. Recv bytes `argdist -H 'r::tcp_recvmsg():int:$retval:$retval>0'`.

* Count transmit kernel traces `bpftrace -e 't:net:net_dev_xmit { @[kstack] = count(); }'`.

---
