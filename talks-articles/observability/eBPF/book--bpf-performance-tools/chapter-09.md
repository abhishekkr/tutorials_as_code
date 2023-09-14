
## Chapter.9 Disk I/O

### Background

* Block I/O Stack

```
 Raw BlockDevice I/O->,  ,>[Volume Manager] ,------------------,  /HostBus\
 [Page Cache]<-->[FS] |  (if used)          | Block Layer      |  |Adapter|
                   |, |  |      |,          |       ,----------+->|Driver |
 [BlockDevice Interface]-' [Device Mapper]->|[Classic] [MultiQ]+->\(SCSI) /
                                            |______Scheduler___|    '------>[Disk]
```

* I/O types for tracing: `R: read`, `W: write`, `M: meta`, `S: sync`, `A: read-ahead`, `F: flush/force-unit-access`, `D: discard`, `E: erase`, `N: none`.

* I/O queued is scheduled in **Block Layer** by Classic (NOOP, Deadline, CFQ) or MultiQueue schedulers. Classic use single request queue with single lock, a perf bottleneck for multi-cores. Multi-queue schedulers are..

> * None: no queueing
> * BFQ (Budget Q Fair Scheduling): allocates bandwidht and I/O time; similar to Completely Fair Queue
> * mq-deadline: blk-mq version of Deadline
> * Kyber: adjusts r/w dispatch queue lengths on perf; so latencies can be met

* BPF to trace disk I/O req details; queued times; latency outliers; latency distribution; disk errors, scsi cmds & timeouts.

* Sample strategy: Basic disk metrics (IOPS with `iostat`); Trace block I/O latency dist & latency outliers (with `biolatency`); Trace individual I/O for patterns as reads queue behind writes (with `biosnoop`).


### Traditional Tools

* `iostat` for per-disk I/O stats (IOPS, throughput, I/O req times & use). Columns for `rrqm/s` (read req queued & merged /sec), `wrqm`, `r/s` (read completed req/sec), `w/s`, `rkB/s` (KBs read from disk /sec), `wkB/s`.

* `perf` tracing queueing of requests `block_rq_insert`, issue to storage `block_rq_issue` & completion `block_rq_complete`. BPF's `biosnoop` for efficient alternative.

* `blktrace` tacing block I/O events. Can cause overload.

* SCSI Logging via `dmesg`; need `sysctl -w dev.scsi.logging_level=0x1b6db6db`.


### BPF Tools

```
 ,--------------------------,
 | App                      |    (tools from BCC & bpftrace)
 |--------------------------|
 | SysCall Interface        |
 |----------,---------------|
 | Rest of  | [VFS        ]-|
 |          | [FileSystems]-|           biopattern, biostacks, bioerr, seeksize,
 |          | [BlockDevice]-|<--biotop, biosnoop, biolatency, bitesize
 | Kernel   | [VolManager ]-|<--mdflush
 |          | [Block Layer] |<--iosched, blkthrot
 |          | [HBA (SCSI) ] |<--scsilatency, scsiresult
 |----------:---------------|
 | Device Drivers           |<--nvmelatency
 '--------------------------'
```

* `biolatency` & `biosnoop` can help analysing cloud env to isolate drives breaking latency SLOs.

```
biolatency -Q 10 1  # includes OS time as well
biolatency -D       # shows histograms for disk separately
biolatency -Fm      # in millisec histogram, sepeate for each I/O flag
```

> Default read-ahead configs can ruin perf for heavy apps on fast disks; can be analyzed with `biosnoop`.

* `biotop -C` as top for disks; `bitesize` tracks size of Disk I/O split by procs. `seeksize` tracking how many sectors seek are requested.

* `biopattern` to identify random or sequential I/O. `biostacks` traces full I/O latency with stacktrace. `bioerr` to trace error details.

* `mdflush` tracing multiple device flush events. `iosched` tracing I/O sched queued time.


### BPF One-liners

* Count block I/O tracepoints `funccount t:block:*`.

* Block I/O errors `trace 't:block:block_rq_complete (args->error) "dev %d type %s error %d", args->dev, args->rwbs, args->error'`.

---
