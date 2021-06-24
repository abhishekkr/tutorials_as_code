
## Chapter.1 Introduction

### 1.1 What are BPF & eBPF

> BPF allows the kernel to run mini programs on system and application events empowering non-kernel developers to enable new system technologies.

* BPF is composed of instruction set, storage objects & helper functions

* Its virtual instruction set spec gets executed by Linux Kernel BPF Runtime; which first verifies BPF program safety to not crash/corrupt kernel; composed of interpreter and JIT compiler

* 3 main uses of BPF: networking, observability, security

* Extended BPF (eBPF) still officially BPF; kernel has one execution engine for classic & extended BPF


### 1.2 What are Tracing, Sampling & Observability

* Tracing/Snooping: event based recording ;as used by BPF tools which can run small programs on events to do real time actions avoiding post-process on bulk

* Sampling/profiling: subset analysis with timer-based (or other ways to chunk) samples

* Observability (o11y): understanding via observing on snoops/profiles; not including benchmark tools


### 1.3 What are BCC, bpftrace & IO Visor

* Frontends to avoid BPF direct coding: `BCC` (BPF Compiler Collection, 1st high level tracing framework) & `bpftrace`

> they use `libbcc` & `libbpf` which in turn hook into Event Sources & Kernel baked BPF

* BCC provides C to write Kernel BPF code & other languages with user-level interface

* bpftrace provides high-level language developing BPF tools; primarily ideal for powerful short scripts/one-liners

* [ply](https://wkz.github.io/ply/) compiles its scripts into Linux BPF programs attached to `kprobes` & `tracepoints` in kernel


### 1.4 First Look at BCC: Quick Wins

* tracing new processes giving one-line summary

```
# sudo ./execsnoop

PCOMM            PID    PPID   RET ARGS
systemd-user-ru  96205  1        0 /usr/lib/systemd/systemd-user-runtime-dir stop 969
```

> `execsnoop` aids perf analysis called `workload characterization`

* `biolatency` on a prod DB sensitive to high latency with SLA to deliver requests; when tool is stopped it prints summary of `block I/O events`


### 1.5 BPF Tracing Visibility

```
+-----------------------------+-------------------+---------------------------+
| Components                  | Traditional       | BPF Tracing               |
+-----------------------------+-------------------+---------------------------+
| App with Language Runtimes  | Runtime debuggers | Yes, with runtime support |
+-----------------------------+-------------------+---------------------------+
| App using compiled code     | System debuggers  | Yes                       |
+-----------------------------+-------------------+---------------------------+
| System libraries (lib/*)    | ltrace            | Yes                       |
+-----------------------------+-------------------+---------------------------+
| System call interface       | strace, perf      | Yes                       |
+-----------------------------+-------------------+---------------------------+
| Kernel: Scheduler/FS/TCP/*  | Ftrace, perf      | Yes, in more detail       |
+-----------------------------+-------------------+---------------------------+
| Hardware: CPU internal, dev | perf, sar, /proc  | Yes, direct or indirect   |
+-----------------------------+-------------------+---------------------------+
```


### 1.6 Dynamic Instrumentation: kprobes & uprobes

* supports multiple event sources

* dynamic instrumentation/tracing is ability to probe into live software

* Linux added dynamic instrumentation for user-level functions in 2012, in form of `uprobes`. BPF uses both `kprobes` & `uprobes`

> examples

```
+------------------------------+----------------------------------------------------+
| Probe                        | Description                                        |
+------------------------------+----------------------------------------------------+
| kprobe:vfs_read              | Instrument beginning of kernel vfs_read() function |
+------------------------------+----------------------------------------------------+
| kretprobe:vfs_read           | Instrument returns of kernel vfs_read() function   |
+------------------------------+----------------------------------------------------+
| uprobe:/bin/bash:readline    | Instrument beginning of readline() fn in /bin/bash |
+------------------------------+----------------------------------------------------+
| uretprobe:/bin/bash:readline | Instrument returns of readline() fn in /bin/bash   |
+------------------------------+----------------------------------------------------+
```


### 1.7 Static Instrumentation: Tracepoints & USDT

* BPF tracing supports kernel static instrumentation, `USDT` (user-level statically defined tracing) for user-level static instrumentation

* Recommended strategy is try static tracing first then switch to dynamic when static is unavailable

```
+-----------------------------------------+------------------------------------+
| Probe                                   | Description                        |
+-----------------------------------------+------------------------------------+
| tracepoint:syscalls:sys_enter_open      | Instrument open(s) syscall         |
+-----------------------------------------+------------------------------------+
| usdt:/usr/sbin/mysqld:mysql:query_start | Instrument query__start probe from |
|                                         |   /usr/sbin/mysqld                 |
+-----------------------------------------+------------------------------------+
```


### 1.8 First Look at bpftrace: Tracing open()

* `sudo bpftrace -l 'tracepoint:syscalls:sys_enter_*'` to list all matching tracepoints using `bpftrace`

* use `bpftrace` to trace `open(2)` syscall using tracepoint `syscalls:sys_enter_open`

```
# sudo bpftrace -e 'tracepoint:syscalls:sys_enter_open { printf("%s %s\n", comm, str(args->filename)); }'

Attaching 1 probe...
postgres pg_stat_tmp/global.stat
postgres pg_stat_tmp/global.tmp
```

> this one gives per event, a line output
>
> if not all `open` events are seen; that's because there are few variants of it and only one traced; all can be listed using `sudo bpftrace -l 'tracepoint:syscalls:sys_enter_open*`

* can do count of all open like tracepoints as following

```
# sudo bpftrace -e 'tracepoint:syscalls:sys_enter_open* { @[probe] = count(); }'
Attaching 5 probes...
^C

@[tracepoint:syscalls:sys_enter_open]: 22
@[tracepoint:syscalls:sys_enter_openat]: 435
```

* the above print doesn't work for tracepoint with wildcard, so print for other separately

```
# sudo bpftrace -e 'tracepoint:syscalls:sys_enter_openat { printf("%s %s\n", comm, str(args->filename)); }'

Attaching 1 probe...
Chrome_IOThread /dev/shm/.com.google.Chrome.YzhU20
abrt-dump-journ /var/log/journal/a8a8b8b8d82d40fd9da2f64e388b2a63/system.journa
```

* doing a detailed/complex logic in one-liner is unmanageable; so executable scripts can be used for it with bpftrace as `opensnoop.bt` which ships along


### 1.9 Back to BCC: Tracing open()

* there is `opensnoop`, BCC version of earlier utility which also provides cli args/switches like `-x` to show only failed opens for better debug



> [Next Chapter 2. Technology Background](./chapter-02.md)

---
