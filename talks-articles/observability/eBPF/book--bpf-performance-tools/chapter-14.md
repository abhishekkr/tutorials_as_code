
## Chapter.14 Kernel


### Background

* Wakeup events for off-CPU threads, like disk I/O. The dependency chain is insightful.

* Mem Alloc: `slab/slub allocator` for objects of fixed sizes; `page allocator` for memory pages (also NUMA aware). API calls like `kmalloc`, `kzalloc`, `kmem_cache_alloc`, `vmalloc`, `vzalloc`, `alloc_pages`.

* Kernel Locks: Spin locks, Mutex (hybrid with 3 acquisition paths which are fastpath `cmpxchg`, midpath of spin first, and slowpath of block until available); RCU (read-copy-update) sync mechanims.

* Device drivers have a half part handling interrupt quickly & scheduling work to other half. Other half tasklets or work-queues threads can be scheduled by kernel or sleep if needed.

* BPF tracing for threads leaving CPU, latencies, off-CPU wait, kernel slab allocator, NUMA, workqueue events, functions called.


### Traditional Tools

* `Ftrace` traceris a multitool. Do Fn counting (also `funccount`), collect stacktraces (also `kprobe`), child Fn charting (also `funcgraph`).

* `perf sched` profiler can do scheduler analysis.

* `slabtop` (slab cache usage); `-s c` to sort by size.


### BPF Tools

```
         ,--------------------------,
         | App                      |    (tools from BCC & bpftrace)
         |--------------------------|
         | SysCall Interface        |
         |--------,-------,---------|<--loads, offcputime, wakeuptime
         | Rest of| Locks |Scheduler|<--workq, offwakeuptime
         | Kernel |       |_________|
         |        |       |Virtual  |<--kmem, memleak
  mlock  |        |       |Memory   |
  mheld--|--------|->     |  [Slabs]|<--slabratetop
         |        |       |  [Pages]|<--kpages, numamove
         |--------:-------:---------|
         | Device Drivers           |
         '--------------------------'
```

* `loads` lists system load avergaes. Code similar to

```
#!bpftrace

#include <linux/sched/loadavg.h>

BEGIN {
    printf("Reading load averages... Hit Ctrl-C to end.\n");
}

interval:s:1 {
    $avenrun = kaddr("avenrun");
    $load1 = *$avenrun;
    $load5 = *($avenrun + 8);
    $load15 = *($avenrun + 16);
    time("%H:%M:%S ");
    printf("load averages: %d.%03d %d.%03d %d.%03d\n",
        ($load1 >> 11), (($load1 & ((1 << 11) - 1)) * 1000) >> 11,
        ($load5 >> 11), (($load5 & ((1 << 11) - 1)) * 1000) >> 11,
        ($load15 >> 11), (($load15 & ((1 << 11) - 1)) * 1000) >> 11
    );
}
```

* `offcputime` showing task state, `TASK_UNINTERRUPTIBLE` is blocked on resources. `offwakeuptime` combines `offcputime` & `wakeuptime`.

```
offcputime -uK --state 2  # 0: TASK_RUNNING, 1: TASK_INTERRUPTIBLE
```

* `mlock` & `mheld` trace mutex lock latency & held times.

---
