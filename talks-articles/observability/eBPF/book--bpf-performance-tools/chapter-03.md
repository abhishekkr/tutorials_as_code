
## Chapter.3 Performance Analysis

### Overview

* measurements include `latency`, `rate`, `throughput`, `utilization`, `cost`

* determine the goal before tapping numbers, otherwise just logging and summary analysis are mostly enough

* BPF perf tools can be used for more than analyzing a issue; like ensuring required latencies are met

* focus on finding what issues are of high priority; and what all causes imapct a given issue


### Performance Methodologies

* `Workload Characterization` with steps `what is causing load`, `why is load called`, `what is load`, `how is load changing with time`

* `Drill-Down Analysis` analyzing top-down looking for load causal hints; tooling is better suited to `bpftrace` than `BCC`

* `USE Method` (Utilization, Saturation, Errors) monitoring ability of tool revealing heavy joints & blindspots

* Perf Analysis `Checklist` beginning with what matters; to focus better


### Linux 60-Second Analysis

* `uptime`; giving `load average` numbers as exponentially damped moving sum averages with 1min/5min/15min constant

* `dmesg | tail`; shows system messages and errors when they occur

* `vmstat 1`; virtual mem stats tool to print 1sec summaries, first line is since boot and there after with counter

> * column `r` for number of processes running/waiting; `free` mem in KBytes
>
> * `si` & `so` for swap-ins/outs
>
> * `us` user time, `sy` system time, `id` idle, `wa` wait I/O, `st` stolen time as breakdown for CPU time

* `mpstat -P ALL 1`; print per-CPU time for thread bottleneck, high %iowait time, high %sys time

* `pidstat 1`; rolling output for CPU usage per process

* `iostat -xz 1`; shows storage i/o metrics with columns to check as

> * `r/s` reads/sec, `w/s` writes/sec, `rkB/s` & `wkB/s` as r/w KB/sec; `await` avg time for I/O in milli-sec
>
> * `avgqu-sz` avg request count issued; `%util` for device utilization with greater value tend to poor perf although confusingly wrong due to parallel work

* `free -m`; shows available mem in MBs

* `sar -n DEV 1`; n/w device metrics checking interface throughput rxkB/s & txkB/s

* `sar -n TCP,ETCP 1`; to look TCP metrics & errors with `active/s`, `passive/s` & `retrans/s` as main columns of data

* `top`



### BCC Tool Checklist

* `execsnoop`; shows new process for every `execve` syscall

* `opensnoop`; shows each `open` syscall & variants, error if any

* `ext4slower` (or `btrfs*`, `nfs*`, `xfs*`, `zfs*`); traces r/w/open/syncs which exceed threshold time

* `biolatency`; traces disk I/O latency and show as histogram

* `biosnoop`; a line for each disk I/O with latency details

* `cachestat`; one-liner summary per-sec, identify a low-cache hit ratio

* `tcpconnect`; every active TCP connection via `connect()` with details

* `tcpaccept`; for every passive TCP connection via `accept()`

* `tcpretrans`; for every TCP retransmit packet (a lot can cause latency/throughput issues)

* `runqlat`; times waiting of threads at CPU & prints it as histogram

* `profile`; is a CPU profiler to catch time taking code paths


> [Next Chapter 4. BCC](./chapter-04.md)

---
