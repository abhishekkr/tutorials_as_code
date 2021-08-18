
## Chapter.2 Technology Background

### 2.1 BPF Illustrated

```
 User                           Kernel
 ==========================   ============================================
                                                         static tracing
 [BPF Program]----------------->[__BTF__]            ,---[Sockets]
    '->[BPF bytecode]<-----------'  ^|,             / ,---[Tracepoints]
                  '--------------->[Verifier]      / / ,---[User Markers]
                                     |,           / / /
  [Event Config].................[_______BPF_____]:::::::.    dynamic tracing
                                   |,        '| |' \   \  \---[kprobes]
<--[Per event data]<-----------[perf buffer]  | |   \   \---[uprobes]
output                                       ,| |,   \
<---[Stats, stacks]<---------->[_Maps__[stacks]_]     \     sampling, PMCs
                                                       '----[perf_events]
```


### 2.2 BPF

* classic BPF was a limited VM with 2 registers, a scratch memory with 16 slots & a program counter

* all operating with 32 bit register size

* implementation later got a JIT-to-native code compiler with interpreter, improving perf


### 2.3 eBPF

* there is no VM layer, JIT compiled code runs directly in Kernel

* eBPF brought changes

> * added more registers (now 10; R0-R9 and R10 as read-only frame pointer)
>
> * switched to 64-bit words
>
> * created infinite flexible BPF map storage; and 512 bytes stack space
>
> * enabled calls to some earlier restricted Kernel code

* BPF programs should be safe and finish in bounded time

* `bitehist` shows size of disk I/O as histogram; when generates histogram is kernel context avoids copying data to userspace & reprocessing hence performant

* BPF programs are **verified** hence safer than kernel-loadable modules; alongwith low skill required to adapt at wider scale

* `LLVM` supports BPF as compilation target; thus theoretically any LLVM supported high-level lang can be used to compile into BPF where LLVM further optimizes BPF instructions

* `bpftool` allow viewing/manipulating BPF objects w/ programs & maps

> * currently `bpftool` operates on `OBJECT := { prog | map | link | cgroup | perf | net | feature | btf | gen | struct_ops | iter }`
>
> * `bpftool perf` shows programs attached via `perf_event_open()`

* `bpftool prog show` lists all programs

```
# sudo bpftool prog show

21: cgroup_device  tag f958f6eb72ef5af6  gpl
	loaded_at 2021-06-15T18:08:38+0530  uid 0
	xlated 456B  jited 276B  memlock 4096B
22: cgroup_skb  tag 6deef7357e7b4530  gpl
	loaded_at 2021-06-15T18:08:38+0530  uid 0
...
...
	xlated 64B  jited 54B  memlock 4096B
1795: tracing  name do_sys_open  tag 871b7e76e78a9993  gpl
	loaded_at 2021-06-18T01:21:11+0530  uid 0
	xlated 592B  jited 433B  memlock 4096B  map_ids 19
	btf_id 9
```

> the `bpftrace` program IDs here are `21,22,..` without `btf_id` attrib & BCC program IDs `1795` with `btf_id` attrib

* dump BPF program instructions in assembly (using `xlated` mode) as

```
# sudo bpftrace prog dump xlated id 21
   0: (61) r2 = *(u32 *)(r1 +0)
   1: (54) w2 &= 65535
...

# sudo bpftool prog dump xlated id 1795
int kretfunc__do_sys_open(long long unsigned int * ctx):
; KRETFUNC_PROBE(do_sys_open, int dfd, const char __user *filename, int flags, int mode, int ret)
   0: (bf) r6 = r1
; KRETFUNC_PROBE(do_sys_open, int dfd, const char __user *filename, int flags, int mode, int ret)
   1: (79) r1 = *(u64 *)(r6 +32)
   2: (7b) *(u64 *)(r10 -304) = r1
...

# sudo bpftool prog dump xlated id 1795 linum
int kretfunc__do_sys_open(long long unsigned int * ctx):
; KRETFUNC_PROBE(do_sys_open, int dfd, const char __user *filename, int flags, int mode, int ret) [file:/virtual/main.c line_num:52 line_col:0]
   0: (bf) r6 = r1
; KRETFUNC_PROBE(do_sys_open, int dfd, const char __user *filename, int flags, int mode, int ret) [file:/virtual/main.c line_num:52 line_col:1]
   1: (79) r1 = *(u64 *)(r6 +32)
   2: (7b) *(u64 *)(r10 -304) = r1
...
```

> * when used with BTF compiled program as `1795`, includes source info
>
> * when appended with `linum` modifier it includes source file & line number info as well
>
> * similarly `opcodes` modifier includes BPF instruction opcodes AND `visual` modifier emits flow graph info in **DOT format**
>
> * `bpftool prog dump jited id <id>` shows machine code to execute

* `sudo bpftool btf dump [prog] id <id>` dumps BTF IDs for BCC, with typedefs & struct info

* `tcpdump` emits BPF instructions with `-d`; `bpftrace` does it with `-v`

#### BPF API

BPF program can only call provided [helper functions](https://github.com/torvalds/linux/blob/master/include/uapi/linux/bpf.h); some of which are

> * `bpf_map_lookup_elem(map,key)`, `bpf_map_update_elem(map,key,val,flags)`, `bpf_map_delete_elem(map,key)`
>
> * `bpf_probe_read(dst,size,src)`, `bpf_probe_read_str(dst,size,src)`, `bpf_trace_printk(fmt,fmt_size,...)`
>
> * `bpf_ktime_get_ns()`, `bpf_spin_lock(lock)`, `bpf_spin_unlock(lock)`, `bpf_get_current_task()`
>
> * `bpf_get_current_pid_tgid()`, `bpf_get_current_comm(buf,buf_size)`, `bpf_get_current_cgroup_id()`
>
> * `bpf_get_stackid(ctx,map,flags)`, `bpf_perf_event_output(ctx,map,data,size)`, `bpf_perf_event_read_value(map,flags,buf,size)`
>
> here, `bpf_probe_read()` helps read kernel memory outside of BPF with safety

* some of BPF syscalls userspace can invoke are

> `BPF_MAP_CREATE`, `BPF_MAP_LOOKUP_ELEM`, `BPF_MAP_UPDATE_ELEM`, `BPF_MAP_DELETE_ELEM`, `BPF_MAP_GET_NEXT_KEY`
>
> `BPF_PROG_LOAD`, `BPF_PROG_ATTACH`, `BPF_PROG_DETACH`, `BPF_OBJ_PIN`

* made syscalls can be inspected as `strace -ebpf <program>`

```
# sudo strace -ebpf bpftrace -e 'tracepoint:syscalls:sys_enter_openat { printf("%s %s\n", comm, str(args->filename)); }'

bpf(BPF_MAP_CREATE, {map_type=BPF_MAP_TYPE_PERF_EVENT_ARRAY, key_size=4, value_size=4, max_entries=8, map_flags=0, inner_map_fd=0, map_name="printf", map_ifindex=0, btf_fd=0, btf_key_type_id=0, btf_value_type_id=0, btf_vmlinux_value_type_id=0}, 120) = 3
Attaching 1 probe...
bpf(BPF_MAP_UPDATE_ELEM, {map_fd=3, key=0x7ffe2583400c, value=0x7ffe25834010, flags=BPF_ANY}, 120) = 0
bpf(BPF_MAP_UPDATE_ELEM, {map_fd=3, key=0x7ffe2583400c, value=0x7ffe25834010, flags=BPF_ANY}, 120) = 0
```

* BPF Program Types specify the type of events that BPF program attaches to & args for events

> * main types: `BPF_PROG_TYPE_KPROBE`, `BPF_PROG_TYPE_TRACEPOINT`, `BPF_PROG_TYPE_PERF_EVENT`, `BPF_PROG_TYPE_RAW_TRACEPOINT`
>
> * other types: `BPF_PROG_TYPE_SOCKET_FILTER`, `BPF_PROG_TYPE_SCHED_CLS`, `BPF_PROG_TYPE_XDP`, `BPF_PROG_TYPE_RAW_CGROUP_SKB`

* BPF Map Types: `BPF_MAP_TYPE_HASH`, `BPF_MAP_TYPE_ARRAY`, `BPF_MAP_TYPE_PERF_EVENT_ARRAY`, `BPF_MAP_TYPE_PERCPU_HASH`, `BPF_MAP_TYPE_PERCPU_ARRAY`, `BPF_MAP_TYPE_STACK_TRACE`, `BPF_MAP_TYPE_STACK`

> more special purpose maps at `bpf.h`

#### BPF Concurrency Controls

* Linux 5.1 added Spin Lock helpers but not available for use in Tracing programs

* Tracing frontends use Per-CPU Hash & Array Map types avoiding corruption by concurrent R/W overlaps causing lost updates

```
# sudo strace -febpf bpftrace -e 'k:vfs_read { @ = count(); }'

strace: Process 921230 attached
[pid 921230] +++ exited with 0 +++
bpf(BPF_MAP_CREATE, {map_type=BPF_MAP_TYPE_PERCPU_ARRAY, ...  <<<--- using PER CPU HASH
```

> `'k:vfs_read { @++; }'` uses normal hash

* using both at same time shows undercounted events in normal hash

```
# sudo bpftrace -e 'k:vfs_read { @cpuhash = count(); @hash++; }'
Attaching 1 probe... ^C

@cpuhash: 9060
@hash: 9003
```

* other used ways are `BPF_XADD` for atomic map update, BPF spin locks `bpf_spin_lock()`, atomic race-free `bpf_map_update_elem()`

#### BPF sysfs Interface

* conventionally `/sys/fs/bpf` mounted virtual FS exposing BPF programs & maps; allows persistent runs event after loaders exit

* user-level can also R/W these maps to interact

* `BPF_PIN_FD` can be used to pin a BPF Object to make persistent at a path & available; [to keep it loaded even after detached](https://facebookmicrosites.github.io/bpf/blog/2018/08/31/object-lifetime.html)

#### BPF Type Format (BTF)

* BTF is a metadata format encoding debugging info for BPF programs/maps/more

* Inspection & Teacing tools use this to pretty-print details instead of raw hex dump

#### BPF CO-RE

* `Compile Once - Run Everywhere` aims compiling to BPF bytecode once so it can be releasable dist; avoid need of BPF compiler be everywhere

#### BPF Limitations

* limited kernel functions that can be called; limits on loops

* `MAX_BPF_STACK` size is 512; unprivileged BPF program instruction count to 4096 & privileged to 1mil


### 2.4 Stack Trace Walking

* BPF provides special map types to record stacktraces; fetched using `frame pointer-based` or `ORC-based` stack walks

#### Frame Pointer-Based Stacks

* head of linked-list of stack frames found in a register `RBP on x86_64`, return address saved at `+8 offset from RBP`

* `AMD` convention; **BUT** `gcc` might default to omit frame pointer using RBP as general-purpose register, breaking stack walk

> fixed by `-fno-omit-frame-pointer` option; although most things at Kernel are default GCC compiled breaking this option

#### debuginfo

* ELF info files in DWARF format; also includes sections containing source & line number

> BPF doesn't support it for stack walking being processor intensive; BCC & bpftools does

#### Last Branch Record (LBR)

* `Intel` convention; can reconstruct stacktrace from recorded branches in hardware with no overhead.. but has limit to depth due to recorded branch limit

> not supported by BPF at time of Book's checking

#### Oops Rewind Capability (ORC)

* debug info devised for Stacktraces; better optimized than DWARF

> implemented in Linux; supported by BPF; not yet for userspace

#### Symbols

> Stacktraces recorded in Kernel as address array are later converted to Symbols by user-level program; mapping might change


### 2.5 Flame Graphs

> can be used to comapre CPU profiles; visualize recorded stacktraces from any profiler/tracer

* Stack Trace: top with current function, move downward with ancestry of parent, and parents of parent

* Profiling Stack Traces: time sampling 10x100s line long stacktraces; `Linux perf profiler` summarizes as call tree showing percent of each path; BCC summarize for each unique trace

* [Flame Graph](https://www.brendangregg.com/flamegraphs.html): an adjacency diagram; most frequent stack as widest tower (check them first)

> Flame Graph properties
>
> * each box represents function in stack
>
> * y-axis shows stack depth; looking bottom up is the code flow
>
> * x-axis spans sample population; left-to-right IS NOT by time but alphabetical sort of frames

* Frame Graph Features: `hue` indicating code type, `stauration` hashed from fn-name, `bgcolor` maps to graph type; `mouse over` revealing occurency percent; `zoom` to inspect narrow frames; and a `search` to highlight or find


### 2.6 Event Sources

_Kernel Tracepoints_

* via `uprobes` Dynamic Instrumentation

> Applications, System Libraries, System Call Interface `syscalls:`

* via `kprobes` Dynamic Instrumentation

> System Call Interface `syscalls:`
>
> VFS, File Systems `btrfs:, ext4:, xfs:`, Volume Manager `jbd2:`, Block Device Interface `block:`
>
> Sockets `sock:, skb:`, TCP/UDP `tcp:, udp:`, IP, Net Device `net:, xdp:`
>
> Scheduler `cpu-clock, cs, migrations, sched: , task:, signal:, timer:, workqueue:`, Virtual Memory `page-faults, minor-faults, major-faults, kmem:, vmscan:, writeback:, huge_memory:, compaction:`
>
> Device Drives `scsi:, irq:`


### 2.7 kprobes

* kprobes provides dynamic instrumentation for any kernel function, live in prod without reboot or special mode runs

* `kretprobes` is a kprobe interface instrumenting function returns & return-values

> timestamps on `kprobes` & `kretprobes` instrumentation on same function can provide function duration

* if Kprobe is for an address already under Ftrace; Ftrace-based kprobe optimization may be available

> Kernel functions skip tracing when listed with `NOKPROBE_SYMBOL() macro`

#### kprobes Interfaces; 3 used these days

* kprobe API

* Ftrace-based via `/sys/kernel/debug/tracing/kprobe_events`

* `perf_event_open()` as used by `perf` tool and recently `BPF tracing`

#### BPF & kprobes

* BCC via `attach_kprobe()` and `attach_kretprobe()`; supports instrumenting beginning of function and function-plus-instruction offset

* bpftrace via `kprobe` and `kretprobe` prob types; support instrumenting beginning of function only

* example of BCC in `vfsstat` tool instrumenting for Virtual File System interface; using hooks like `b.attach_kprobe(event="vfs_read", fn_name="do_read")`

* following is bpftrace example counting invocation of VFS functions matching `vfs_*`

```
% sudo bpftrace -e 'kprobe:vfs_* { @[probe] = count() }'
Attaching 67 probes...
^C

@[kprobe:vfs_fallocate]: 1
@[kprobe:vfs_fstat]: 1
@[kprobe:vfs_fsync_range]: 3
...
```

> similar to using `funccount` tool from Ftrace perf-trace

> references: [kprobes kernel doc](https://www.kernel.org/doc/Documentation/kprobes.txt); [kprobes intro](https://lwn.net/Articles/132196/), [kernel debugging with kprobes](https://www.ibm.com/developerworks/library/l-kprobes/index.html)


### 2.8 uprobes

* provide user-level dynamic instrumentation; when a function in an executable file is executed then all processes using that file are instrumented

* has `uprobes` for func-entries (with instruction offset) & `uretprobes` (for return of func); also implemented similar to kprobes

> to instrument readline function with bpftrace `sudo bpftrace -e 'uprobe:/bin/bash:readline { @ = count() }'`

#### 2 uprobes Interfaces

* Ftrace based via `/sys/kernel/debug/tracing/uprobe_events`

* `perf_event_open()`; with `perf_uprobe` pmu

> `register_uprobe_event()` kernel function is available, not exposed as an API though

#### BPF & uprobes

* BCC probes as `attach_uprobe()` (can instrument beginning or with offset) & `attach_uretprobe()`

> example of BCC at `gethostlatency` instruments host resolution calls; using calls like `b.attach_uprobe(name="c". sym="getaddreinfo", fn_name="do_entry", pid=args.pid)`

* bpftrace probe types as `uprobe` (only beginning) & `uretprobe`

> example of bpftrace as `bpftrace -l 'uprobe:/lib/x86_64-linux-gnu/libc.so.6:gethost*'`

#### uprobes Overhead & Future work

* attaching uprobes to million/sec fired events (like malloc, free) can cause slowdowns even if BPF is optimized; such uses are prohibited

> shared-lib solution is in [discussion](https://events.static.linuxfound.org/images/stories/pdf/eeus2012_desnoyers.pdf) to provide BPF tracing for user-space without kernel mode switch

> references: [uprobes kernel doc](https://www.kernel.org/doc/Documentation/trace/uprobetracer.txt)


### 2.9 Tracepoints

* used for Kernel static instrumentation; involve tracing calls placed in kernel by developers

> thus need maintenance but provide stable API across kernel versions; unlike kprobes which might break on function rename

* format of tracepoint is `subsystem:eventname` (e.g. `kmem:kmalloc`)

#### Adding Tracepoint Instrumentation

> header file for tracepoint `sched.h` under `include/trace/events` define trace system as *sched* and tracepoint named *sched_process_exec*

* info available at runtime via `Ftrace` framework in `/sys`, via format files for each tracepoint

```
$ cat /sys/kernel/debug/tracing/events/sched/sched_process_exec/format

name: sched_process_exec
ID: 309
format:
	field:unsigned short common_type;	offset:0;	size:2;	signed:0;
...
```

#### 2 Tracepoint Interfaces

* Ftrace-based via `/sys/kernel/debug/tracing/events` has sub-dirs for each tracepoint system, files for each tracepoint itself

* `perf_event_open()`, by BPF via `perf_tracepoint` pmu

#### Tracepoints and BPF

* BCC `TRACEPOINT_PROBE()` and bpftrace with `tracepoint` probe type

* `tcplife` is an example of BCC and tracepoints with `BPF.tracepoint_exists("sock", "inet_sock_set_state")` to use `bpf_text_tracepoint` if exists

* bpftrace one-liner `bpftrace -e 'tracepoint:sched:sched_process_exec { printf("exec by %s\n", comm); }'`

```
% sudo bpftrace -e 'tracepoint:sched:sched_process_exec { printf("exec by %s\n", comm); }'
Attaching 1 probe...
exec by sh
exec by git
^C
```

#### BPF Raw Tracepoints

* `BPF_RAW_TRACEPOINT` (added Linux 4.17) avoids creation cost for stable tracepoint arguments; making unstable API with access to more fields

> references: [tracepoints kernel doc](https://www.kernel.org/doc/Documentation/trace/tracepoints.rst), [48](https://lkml.org/lkml/2018/2/28/1477)


### 2.10 USDT

* USDT (User-level Statically Defined Tracing) provides userspace version of Tracepoints

* USDT depends on external system tracer to activate; made popular by DTrace

> many apps don't compile USDT probes by default but require config option

#### Adding USDT Instrumentation

* can be added to an app using headers and tools from `systemtap-sdt-dev`, Facebook's [folly](https://github.com/facebook/folly)

#### How USDT Work

* similar to others, when apps are compiled a `nop` is placed at address of USDT probes, then dynamically changed by kernel to a breakpoint when instrumented using uprobes

#### BPF and USDT

* BCC with `USDT.enable_probe()` and bpftrace with `usdt` probe type

> references: [Hacking Linux USDT with Ftrace](http://www.brendangregg.com/blog/2015-07-03/hacking-linux-usdt-ftrace.html), [USDT probe support in BPF/BCC](http://blogs.microsoft.co.il/sasha/2016/03/30/usdt-probe-support-in-bpfbcc/), [USDT tracing report](http://blog.srvthe.net/usdt-report-doc/)


### 2.11 Dynamic USDT

* some languages are run on the fly; dynamic USDT can be used there

* pre-compiling a shared lib with desired USDT probes embedded in functions (with ELF notes section for USDT probes); this wrapped and used

> * `libstapsdt` auto creates shared lib containing USDT probes and ELF notes section at runtime
>
> * `libusdt` being worked on for it


### 2.12 PMCs

* PMC (Perf Monitoring Counters) also known as PICs (Perf Instrumentation Counters), CPCs (CPU Perf Counters), PMU (Perf Monitoring Unit) events... are all just **Programmable Hardware Counters on Processor**

* there are many PMCs; Intel selected 7 as architectural set

* only PMCs allow measure CPU instructions efficiency; CPU cache hit rations; memory, interconnect and device bus utilization; stall cycles; more

* there are 100s of PMCs available; but only a fixed (near to 6) can be chosen at a time to measure

#### 2 PMC Modes

* `Counting`: PMCs track event rate; with almost zero overhead

* `Overflow Sampling`: PMCs interrupt kernel at monitored events to collect extra state; might cause serious issues based on occurence rate

* PEBS (Precise Event Based Sampling) uses h/w buffers to record correct instruction pointer at time of PMC event (to manage overflow sampling)

#### Cloud Computing

* many Cloud Providers have not provided PMC access to guests



> [Next Chapter 3. Performance Analysis](./chapter-03.md)

---
