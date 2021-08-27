
## Chapter.4 BCC

### BCC Components

* [BCC](https://github.com/iovisor/bcc) contains doc/man-pages/eg-files; provides interface for developing BCC tools in Python, C++ and Lua

* general tool location on install is `/usr/share/bcc/tools`


### BCC Features

#### Kernel-Level Features

* Dynamic Instrumentation, kernel-level via `kprobes` & user-level via `uprobes` for BPF

* Static Tracing, kernel-level via `tracepoints`

* Timed sampling events & PMC events with `perf_event_open()`

* Filtering (`BPF programs`), Debug output (`bpf_trace_printk()`), Per-event output (`bpf_perf_event_output()`)

* Basic Variables, Associative Arrays, Freq Counting & Histograms via BPF maps

* Timestamps & time deltas (`bpf_ktime_get_ns()` & BPF programs)

* Stack traces, kernel & user via BPF stackmap

* Overwrite ring buffers (`perf_event_attr.write_backward`)

* Low-overhead instrumentation (`BPF JIT`, BPF map summaries)

* Production Safe due to `BPF verifier`

#### BCC User-Level Features

* Static Tracing (USDT probes via uprobe)

* Symbol resolution; kernel-level (`ksym()` & `ksymaddr()`) and user-level (`usymaddr()`)

* Debug output (Python with `BPF.trace_pipe()` and `BPF.trace_fields()`); Debuginfo symbol resolution support

* `BPF_PERF_OUTPUT` macro & `BPF.open_perf_buffer()` for Per-event Output

* `BPF.get_table()` and `table.clear()` for Interval output

* `table.print_log2_hist()` for Histogram printing

* C struct navigation, `bpf_probe_read()`

> many other macros & functions


### BCC Installation

* [install doc](https://github.com/iovisor/bcc/blob/master/INSTALL.md)

> common system location for `bcc-tools`: `/usr/share/bcc/{tools,man/man8}`


### BCC Tools

#### Highlighted Tools

> covered in later chapters

* Debugging/multi-purpose: trace, argdist, funccount, stackcount, opensnoop

* CPUs: execsnoop, runqlat, runqlen, cpudist, profile, offcputime, syscount, softirq, hardirq

* Memory: memleak

* File-systems: opensnoop, filelife, vfsstat, fileslower, cachestat, writeback, dcstat, xfsslower, ext4dist

* Disk I/O: biolatency, biosnoop, biotop, bitesize

* Networking: tcpconnect, tcpaccept, tcplife, tcpretrans

* Security: capable

* Languages: javastat, javacalls, javathreads, javaflow, javagc

* Applications: mysqld\_qslower, signals, killsnoop

* Kernel: wakeuptime, offwaketime

> Primary language used is Python for user-level, C for kernel-level.
>
> Checking comment section of all these tools give a quick insight

### funccount

* counts events of func/tracepoints/USDT calls; for high-freq events can cause overhead compared to post-process tools

```
# examples

% funccount tcp_drop

% funccount 'vfs_*'    ## VFS kernel calls

% funccount 'tcp_*'    ## TCP kernel calls
% funccount -i 1 'tcp_send' ## tcp send calls per second
% funccount -i 1 't:block:*'   ## rate of block I/O per-sec
% funccount -i 1 't:sched:sched_process_fork'   ## rate of new process per-sec
% funccount -i 1 'c:getaddrinfo'   ## rate of libc getaddrinfo() per-sec
% funccount -i 1 'go:os*'   ## count all "os.*" calls in libgo

% funccount -r '^vfs.*' ## matching using regex
% funccount -Ti 5 'vfs_*' ## output every 5sec, with timestamps
% funccount -d 10 'vfs_*' ## trace for only 10sec
% funccount -p 181 'vfs_*' ## count vfs calls for PID 181 only


### rate of user-level `pthread_mutex_lock()` fn/sec
% funccount -i 1 c:pthread_mutex_lock
```


### stackcount

* counts stack traces that led to an event

* done entirely in kernel context using a special BPF map; userspace reads stack IDs & counts

```
% stackcount ktime_get   # identify the code paths that led to ktime_get

% stackcount -P ktime_get  # includes process name and PID with stack trace
```

* can use [FlameGraph](https://github.com/brendangregg/FlameGraph) to visualize output from traces of `stackcount -f -P -D 10 ktime_get > /tmp/out` by `./flamegraph.pl --hash --bgcolor=grey < /tmp/out > /tmp/flame.svg`


### trace

* a per-event tracing BCC multi-tool from different sources (kprobes, uprobes, tracepoints, USDT)

* to find out what args passed; return value; if failing; how is function called

```
% trace 'do_sys_open "%s", arg2'   ## shows file opens

% trace 'r::do_sys_open "ret: %s", retval'   ## return of kernel do_sys_open

% trace -U 'do_nanosleep "mode: %d", arg2'   ## trace do_nanosleep with mode & user-level stacks

% trace 'pam:pam_start "%s: %s", arg2, arg2' ## trace auth requests via pam library
```

* BCC uses system headers to understand some structs

```
% trace 'do_nanosleep(struct hrtimer_sleeper *t) "task: %x", t->task' ## hrtimer read from kernel headers
```

* trace debugging File Descriptor Leaks; output of below can be processed to checked sockets opened but not closed

```
% trace -tKU 'r::sock_alloc "open %llx", retval' '__sock_release "close %llx", arg1'
```


### argdist

* a multi-tool summarizing arguments

```
% argdist -H 'r::__tcp_select_window():int:$retval'  ## checking zero-sized window adverts

% argdist -H 'r::vfs_read()'  ## histogram of results

% argdist -p 1005 -H 'r:c:read()'   ## histogram of results returned by user-level libc read for PID 1005

% argdist -C 't:raw_syscalls:sys_enter():int:args->id'  ## syscalls by syscall ID

% argdist -p 181 -C 'p:c:write(int fd):int:fd'  ## libc write() call for PID 181

% argdist -C 'r::__vfs_read():u32:$PID:$latency > 100000'   ## print freq of reads by process when latency >0.1ms
```


### Tool Documentation

* man page

```
bcc-man(){
  local toolname="$1"
  cp "/usr/local/bcc/man/man8/${toolname}.8.gz" "/tmp/${toolname}.8.gz"
  gunzip "/tmp/${toolname}.8.gz"
  nroff -man "/tmp/${toolname}.8" | less
}
```


### Developing BCC Tools

* BCC allows lower level of control for BPF programs in C; for user-level components in Python


### BCC Internal

* C++ front-end API for kernel-level including preprocessor converting mem dereferences to `bpf_probe_read()`

* C++ back-end drivers; compiling via Clag/LLVM; loading BPF program; attaching to events; r/w with BPF maps

* Python/C++/Lua front-end for composing BPF tools

> * BPF, Table & USDT python objects are wrappers to implementation in `libbcc` & `libbcc_bpf`
>
> * USDT (separate object in Python); behavior differs from kprobes/uprobes/tracepoints as must be attached to process ID/path during initialization because USDT need semaphores be set for activation

* Steps by BCC to load BPF:

> * python BPF object creared & BPF C program passed to it
>
> * BCC rewriter preprocesses BPF C program; BCC codegen adds additional LLVM IR
>
> * required Maps created; bytcode sent to kernel for BPF verifiere; events enabled and attached
>
> * BCC program reads instrumented data via maps or per\_event buffer


### BCC Debugging

#### `printf()` Debugging

* added to Python code; also BPF code `bpf_trace_printk()` emitting output to special Ftrace buffer (cat `/sys/kernel/debug/tracing/trace_pipe` files)

* e.g. adding `bpf_trace_printk("SOME_UNIQ_FLAG req=%11x ts=%11d\\n", req, ts);` to `trace_req_start()` in `biolatency`; running it; then `cat /sys/kernel/debug/tracing/trace{,_pipe}` for lines with `SOME_UNIQ_FLAG`

> * `trace` file here prints header, doesn't block
>
> * `trace_pipe` blocks for more messages & clears messages as read

#### BCC Debug Output

* some tools have debug command line switch as `-D`; available in `-h`

* `--ebpf` option prints final BPF program that tool is to generate

#### BCC Debug Flag

* BCC debugging capability can be added via debug flag by changing BPF object init in code

```
## changing following
b = BPF(text=bpf_text)

## to below, makes it print BPF instructions
b = BPF(text=bpf_text, debug=0x2)
```

> * debug option `0x1` is `DEBUG_LLVM_IR`; `0x2` is `DEBUG_BPF`
>
> * `0x4` is `DEBUG_SOURCE` for ASM instructions embedded with source
>
> * `0x8` is `DEBUG_BPF_REGISTER_STATE`; `0x20` is `DEBUG_BTF` (BTF errors are otherwise ignored)

#### bpflist, bpftool, dmesg

* `bpflist` lists tools that have running BPF programs along with details

* `bpftool` can show running programs, list BPF instructions, interact with map, etc.

* `dmesg` view kernel error in system log

#### Resetting Events

* introducing bugs may leave BCC tools/libs crashed & kernel event sources enabled

* Linux 4.17, BCC switched using `perf_event_open()` that cleans-up on crash


> [Next Chapter 5. bpftrace](./chapter-05.md)

---
