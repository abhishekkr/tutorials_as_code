
## Chapter.7 Memory

### Background

#### Memory Allocators

```
                                                          VIRTUAL.|.PHYSICAL
  User-Level             ,--------------------------------------, |
 ,--------,              | [Kernel]   ,-------,  ,------------, | |
 | [Proc] |  ,--------,  | Modules    |[Slab] |  |[Page Alloc]|-+---[DRAM]
 |Segments|  | [libc] |  | <ext4>-----|[Alloc]|  |            | | |
 | ,-----,|  | [Alloc]|  | <scsi,..>--| Caces |--| Free Lists |-+---[DRAM]
 | | Heap|+--| Memory |  |            |_______|  |            | | |
 | '-----'|  | [][][] |--+-----------------------'------------'-+---[DRAM]
 '--------'  '--------'  '--------------------------------------' |

 (of processes using libc for mem alloc)
```

* Kernel & Processor map virtual memory to physical memory. Kernel can serve phy. mem-page requests using its free lists maintained for DRAM groups & CPU. Kerne allocators like Slab Allocator help kernel use these mem.

#### Lifecycle of Memory Page

> * Mem allocation request
> * Allocator either extend heap (using `brk`) or creates new mem segment
> * When accessed, MMU helps translate virtual to physical mapping. MMU error (Page Fault) for no mapping of address. Kernel notifies MMU of new mapping for later lookups. Mem size in use is RSS (Resident Set Size).
> * `kswapd` frees mem pages (either of unmodified since read from disk, dirty pages that need be backed, app memory) on heavy demand.

* Generally `brk`, `mmap`, page fauls & page outs should be infrequent. Tracing these would have less overhead.

#### Page-out Daemon

* `kswapd` regularly scans LRU lists, works when mem cross low threshold. If `kswapd` isn't quick enough, direct reclaim gets used calling kernel module shrinker functions on caches..

> * OOM killer is last resort to free mem; looks largest victim to free most pages.
> * Kernel use page compaction routine to avoid fragmentation.

#### BPF Capabilities

> Why RSS keep growing? What code paths causing page faults? What proc blocked on swap-ins?
> What mem mapping are system wide? System state at OOM? Paths allocating mem. Potential mem leaks.

* Event Sources
> * User mem allocations: uprobes on allocator fn, libc USDT probes
> * Kernel mem allocations: kprobes on alloc fn, kmem tracepoints
> * Heap expansions: brk syscall tracepoint
> * Shared mem fn: syscall tracepoints
> * Page faults: kprobes, s/w events, exception tracepoints
> * Page migrations: migration tracepoint; Page compaction: compaction tracepoint
> * Virutal Mem scanner: vmscan tracepoint; Mem access cycles: PMCs


### Traditional Tools

> * Check for OOM; `dmesg`
> * Check swap and active I/Os; `swapon`, `iostat`, `vmstat`
> * Check free mem, cache usage; `free`
> * Per-proc mem usage; `top`, `ps` (col VSZ is Virtual Mem Size), `pmap`
> * Page faults and related stacktraces (can explain RSS growth). Check files backing page faults; `sar`
> * Trace `brk` & `mmap` calls. Measure h/w cache misses & mem access for Fn causing I/O; `perf`

```
sar -B 1  ## shows page stats /sec; like page outs & page faults
perf stat -e LLC-loads,LLC-load-misses -a -I 1000  ## measure LLC hit/miss
```


### BPF Tools

```
 ,--------------------------,
 | App                      |    (tools from BCC & bpftrace)
 |     ,--------------------|
 |     |  System Libs       |<--memleak
 |--------------------------|
 | SysCall Interface        |<--mmapsnoop, brkstack
 |----------,---------------|
 | Rest of  | Virtual       |<--oomkill, shmsnoop, vmscan, drsnoop
 |          | Memory        |   memleak         ,---------,
 | Kernel   |               |-------------------[MMU] CPU |
 |----------:---------------|    'faults,       '---------'
 | Device Drivers           |     ffaults, hfaults,
 '--------------------------'     swapin
```

* BCC's `oomkill` trace OOM kill events, similar to

```
#!bpftrace

#include <linux/oom.h>

BEGIN {
  printf("Tracing oom_kill_process()... Hit Ctrl-C to end.\n");
}

kprobe:oom_kill_process {
  $oc = (struct oom_control *)arg1;
  time("%H:%M:%S ");
  printf("Triggered by PID %d (\"%s\"), ", pid, comm);
  printf("OOM kill of PID %d (\"%s\"), %d pages, loadavg: ",
          $oc->chosen->pid, $oc->chosen->comm, $oc->totalpages);
  cat("/proc/loadavg");
}
```

* BCC's `memleak` trace mem alloc & free events; showing long-term alloc. Can attach to PID. May cause load.

* BCC's `mmapsnoop` traces syscall `mmap`. `brkstack` traces syscall `brk` showing heap expansions. These should be infrequent & low overhead.

* BCC's `shmsnoop` traces SysV shared syscalls `shmget, shmat, shmdt, shmctl`

* `vmscan` shows total time in shrink slab, direct reclaim, cgroup reclaim, kswapd wakeups & page writes.

* `drsnoop` trace diret reclaim approach, proc affected & altency. `swapin` for mem coming in swap.

* `ffaults` for Page Faults; `hfaults` for HugePage Faults.


### BPF One-liners

* `stackcount -U t:syscalls:sys_enter_brk` to count BRK

* `stackcount -U t:exceptions:page_fault_user` to count page faults

* `funccount 't:vmscan:*'` count vmscan ops

* `trace hugepage_madvise` counting HugePage madvise calls

* `bpftrace -e 'software:page-fault:1 { @[comm, pid] = count(); }'` counting page faults by process

---
