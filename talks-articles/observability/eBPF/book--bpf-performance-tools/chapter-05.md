
## Chapter.5 bpftrace

> tracer built on BPF & BCC; ideal for ad-hoc instrumentation with custom one-liners
>
> [reference guide](https://github.com/iovisor/bpftrace/blob/master/docs/reference_guide.md), [kernel analysis with bpftrace](https://lwn.net/Articles/793749/), [cheatsheet](https://www.brendangregg.com/BPF/bpftrace-cheat-sheet.html), [one-liner tuts](https://github.com/iovisor/bpftrace/blob/master/docs/tutorial_one_liners.md)


### Components

* contains doc, man-pages, examples; included bpftrace tools have `.bt` extension

* frontend uses lex & yacc to parse bpftrace, Clang for parsing structure; backend compiles to LLVM IR then BPF by LLVM libs


### Features

#### Event Sources

* Dynamic instrumentation: kernel-level `kprobe` and user-level `uprobe`

* Static instrumentation: kernel-level `tracepoint. software` and user-level `usdt via libbcc`

* Timed sampling events `profile`; Interval events `interval`

* PMC events `hardware`; Synthetic events `BEGIN, END`

#### Actions

* Filtering `predicates`; Per-event output `printf()`

* Basic Variables `global, $local, per[tid]`; Built-in Variables `pid, tid, comm, nsecs, ..`

* Associative Arrays `key[val]`; Frequency Counting `count(), ++`; Statistics `min(), max(), sum(), avg(), stats()`

* Histograms `hist(), lhist()`; Timestamps & deltas `nsecs and hash storage`

* Stack traces `kstack` for kernel & `ustack` for userspace

* Symbol resolution `ksym(), kaddr()` for kernel & `usym(), uaddr()` for userspace

* C struct navigation `->`; Array access `[]`; Shell commands `system()`

* Printing file `cat()`; Positional params `$1, $2, ..`

#### Compared to other Observability Tools

* `perf` has verbose language; supports efficient event dumping in binary format; in-kernel supparies limited to counts

* `Ftrace` has fewer dependencies; has instrumentation mode as function counts

* `Systemtap` adds its own kernel modules, proven unreliable on systems other than RHEL

* `LTTng` has optimized event dumping & analysis tools of dump

* `Application tools` limited to user-level visibility; like JVM profiler & MySQL DB profiler


### Installation

> [documentation with steps](https://github.com/iovisor/bpftrace/blob/master/INSTALL.md)


### Tools

* [list of tools with example links](https://github.com/iovisor/bpftrace#tools)

#### Highlighted Tools

* CPU: execsnoop, runqlat, runqlen, cpuwalk, offcputime

* Memory: oomkill, failts, vmscan, swapin

* File Systems: vfsstat, filelife, xfsdist

* Storage I/O: biosnoop, biolatency, bitesize, biostacks, scsilatency, nvmelatency

* Networking: tcpaccept, tcpconnect, tcpdrop,  tcpretrans, gethostlatency

* Security: ttysnoop, elfsnoop, setuids

* Languages: jnistacks, javacalls

* Applications: threadsnoop, pmheld, naptime, mysqld\_qslower

* Kernel: mlock, mheld, kmem, kpages, workq

* Containers: pidnss, blkthrot

* Hypervisors: xenhyper, cpustolen, kvmexits

* Debugging/multi-purpose: execsnoop, threadsnoop, opensnoop, killsnoop, signals


### One-Liners

* list probes: `sudo bpftrace -l 'tracepoint:syscalls:sys_enter_*'`

* whose executing what

```
% sudo bpftrace -e 'tracepoint:syscalls:sys_enter_execve { printf("%s %s\n", comm, str(args->argv)); }'
Attaching 1 probe...
vim /home/username/bin/go-1.16.7/bin/sh
...
```

* new processes with args

```
% sudo bpftrace -e 'tracepoint:syscalls:sys_enter_execve { join(args->argv); }'
Attaching 1 probe...
git ls-files --error-unmatch --full-name -z -- chapter-05.md
/bin/bash -c /usr/bin/zsh
...
```

* files opened: `sudo bpftrace -e 'tracepoint:syscalls:sys_enter_openat { printf("%s %s\n", comm, str(args->filename)); }'`

* count syscalls by program: `sudo bpftrace -e 'tracepoint:raw_syscalls:sys_enter { @[comm] = count(); }'`

* count syscalls by process: `sudo bpftrace -e 'tracepoint:raw_syscalls:sys_enter { @[pid, comm] = count(); }'`

* count syscalls by probe name: `sudo bpftrace -e 'tracepoint:syscalls:sys_enter_* { @[probe] = count(); }'`

* total `read()` bytes by process: `sudo bpftrace -e 'tracepoint:syscalls:sys_exit_read /args->ret/ { @[comm] = count(); }'`

* distribution of `read()` bytes by process: `sudo bpftrace -e 'tracepoint:syscalls:sys_exit_read { @bytes = hist(args->ret); }'`

> filtered by PID 6763 would be

```
% sudo bpftrace -e 'tracepoint:syscalls:sys_exit_read /pid == 6763/ { @bytes = hist(args->ret); }'
Attaching 1 probe...
^C
@bytes:
[0]                    5 |@                                                   |
[1]                    4 |                                                    |
...
```

* Kernel dynamic-tracing of read bytes `sudo bpftrace -e 'kretprobe:vfs_read { @bytes = lhist(retval, 0, 2000, 200); }'`

> above `lhist()` takes `value, min, max, step` as params for histogram

* timing `read()` in nanoseconds summarized by process name

```
% sudo bpftrace -e 'kprobe:vfs_read { @start[tid] = nsecs; } kretprobe:vfs_read /@start[tid]/ { @ns[comm] = hist(nsecs - @start[tid]); delete(@start[tid]); }'
Attaching 2 probes...
^C
@ns[systemd-journal]:
[4K, 8K)               1 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|

@ns[libinput-connec]:
[1K, 2K)               2 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|
[2K, 4K)               0 |                                                    |
[4K, 8K)               0 |                                                    |
[8K, 16K)              2 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|
...
```

* histogram of block I/O requests by size in bytes: `sudo bpftrace -e 'tracepoint:block:block_rq_issue { @ = hist(args->bytes); }'`

* trace disk I/O size by process

```
% sudo bpftrace -e 'tracepoint:block:block_rq_issue { printf("%d %s %s\n", pid, comm, str(args->bytes)); }'
Attaching 1 probe...
6489 kworker/u16:1
6489 kworker/u16:1
419 NetworkManager
```

* count pages paged in by process: `sudo bpftrace -e 'software:major-faults:1 { @[comm] = count(); }'`

* count page faults by process: `sudo bpftrace -e 'software:faults:1 { @[comm] = count(); }'`

* profile user-level stacks at 99Hz: `sudo bpftrace -e 'profile:hz:45 { @[ustack] = count(); }'`

> * profile FOR 1second: `sudo bpftrace -e 'profile:hz:45 { @[ustack] = count(); }  interval:s:1 { exit(); }'`
>
> * profile FOR pid 6763: `sudo bpftrace -e 'profile:hz:45 /pid == 6763/ { @[ustack] = count(); }'`

* count process level events for 5sec `sudo bpftrace -e 'tracepoint:sched:sched* { @[probe] = count(); } interval:s:5 { exit(); }'`

* count stacktraces leading to context-switch (off-CPU) events: `sudo bpftrace -e 'tracepoint:sched:sched_switch { @[kstack] = count(); }'`


### Programming

* [Kernel Struct Tracing code example](chapter-05-kernel-struct-tracing.bt)

```
% sudo bpftrace chapter-05-kernel-struct-tracing.bt
Attaching 1 probe...
open path: meminfo
open path: statm
...
```

* [Measure time in vfs\_read() kernel function](chapter-05-measure-vfs-read-time.bt)

> can be run with `bpftrace`; or just make executable if shebang is present

#### Program Structure

* series of probes with associated actions and optional filters as `probes { actions }`, `probe /filter/ { actions }`

* or just `/pattern/ { actions }`

* comments are C-like `//` & `/* ... */`

* C struct navigation `->`; C structs can be declared in some cases

* probe format `type:identifier[:identifier[...]]`; multi-purpose probes can be specified with comma-separators to execute same actions

* probe wildcards like `kprobe:vfs_*`; instrumenting too many probes may cost bad overhead so managed by `BPFTRACE_MAX_PROBES` env var *default:512*

* filter like equal `/pid == 1/`; or just `/pid/` to ensure it's non-zero; boolean operators are allowed as `/pid > 100 && pid < 1000/`

* action can be multiple separated by semicolons

* `BEGIN{ action }` &/or `END{ action }` blocks are allowed in one-liner and indented code blocks

* built-in functions be `printf()`, `exit()`, `str(char *)`, `system(format [, args ..])` (run cmd at shell)

* built-in variables are usually read-only; include `pid` for proc-id, `comm` for name, `nsecs` for timesamp, `curtask` for address of current thread's `task_struct`

* scratch variables used for temp calculations having prefix `$`; name and type is set on first assignment

* map variables use BPF map storage object having prefix `@`; key can be provided with one/more elements using maps as a hash table

```
@start[tid] = nsecs;  // simple association

@path[pid, $fd] = str(arg0);  // multi-key association
```

* map functions are special functions that can be assigned to maps or operate on them

```
@a = count();  // using per CPU map becoming special object of type count; allowing @a++

@b = sum($a);  // sums $a variable, when printed gives total

@c = hist($a);  // stores $a in a power-of-two histogram, printing bucket counts & ASCII histogram

print(@x); // printing map @x; all maps are auto-printed on BPF termination

delete(@start[tid]);  // deletes given key & its value pair

clear(@x);  // clears map @x
```


### Usage

* running `bpftrace` shows its help; `-d` && `-dd` for debug dry-run & verbose debug

> `-o file` to redirect output to a file
>
> `-e program` execute a program block
>
> `-p PID` enable USDT probes on PID


### Probe Types

#### tracepoint

* tracepoints usually provide args (for info); e.g. `tracepoint:net:netif_rx` have field `len` for packet length accessible using `args->len`

* `tracepoint:syscalls:sys_enter_read` & `tracepoint:syscalls:sys_exit_read` insrument start and end of `read()` system calls

* its args can be checked using `-l` for list & `-v` for verbose; as

```
% sudo bpftrace -lv 'tracepoint:syscalls:sys_enter_read'
tracepoint:syscalls:sys_enter_read
    int __syscall_nr
    unsigned int fd
    char __attribute__((user)) * buf
    size_t count

% sudo bpftrace -lv 'tracepoint:syscalls:sys_exit_read'
tracepoint:syscalls:sys_exit_read
    int __syscall_nr
    long ret
```

* trace enter/exit of `clone()` syscall that creates new processes; it might have multiple exits/returns (for parent & child processes)

```
% sudo bpftrace -e 'tracepoint:syscalls:sys_enter_clone {printf("-> clone() by %s PID %d\n", comm, pid); }
                      tracepoint:syscalls:sys_exit_clone { printf("<- clone() return %d, %s PID %d\n", args->ret, comm, pid); }'
Attaching 2 probes...
-> clone() by vim PID 1216
<- clone() return 9334, vim PID 1216
<- clone() return 0, vim PID 9334
```

* show `execve()` syscall `sudo bpftrace -e 't:syscalls:sys_enter_clone {printf("%s \t %s; PID %d\n", probe, comm, pid); }'`

#### usdt

* can instrument binaries/shared-libs; `sudo bpftrace -p ${PID} -l` shows all probes available over a Process (grep for `usdt`)

```
% sudo bpftrace -p 1087 -l | grep usdt ## for shell process
usdt:/proc/1087/root/usr/lib/ld-2.33.so:rtld:init_complete
usdt:/proc/1087/root/usr/lib/ld-2.33.so:rtld:init_start
...
```

#### kprobe and kretprobe

* arguments for `kprobe` are `arg0, arg1, ...argN` as unsigned 64-bit int; if pointer to a C struct then cast it (future BTF makes it automatic)

* arguments for `kretprobe` is `retval` built-in for return value which is also uint64

#### uprobe and uretprobe

* similar to usdt, binary/lib path with function name as `uprobe:/bin/bash:readline` & `uretprobe:/bin/bash:readline`

* similar to kprobe/kretprobe; `arg0, arg1..` arguments for `uprobe` & `retval` for `uretprobe`

#### software and hardware

* format of `software:event_name:count`, `software:event_name:`, `hardware:event_name:count`, `hardware:event_name:`

* s/w events similar to tracepoints but suited for count; h/w events are selection of PMCs for processor-level analysis

* to avoid overhead of too many events; sampled on count

* available s/w & h/w events depend on kernel version; some are

> Software Events
>
> * `cpu-clock` (alias: cpu, default count: 1000000); cpu wall-time clock
>
> * `task-clock` (default: 1000000); cpu task clock (increments only when task is on-CPU)
>
> * `page-faults` (alias: faults, default: 100); page faults
>
> * `context-switches` (alias: cs, default: 1000); context switches
>
> * `cpu-migrations` (default: 1); cpu thread migrations
>
> * `minor-faults` (default: 100); Minor page faults: satisfied by memory
>
> * `major-faults` (default: 1); Major page faults: satisfied by storage I/O
>
> * `alignment-faults` (default: 1); Alignment faults
>
> * `emulation-faults` (default: 1); Emulation faults
>
> * `dummy` (default: 1); Dummy event for testing
>
> * `bpf-output` (default: 1); BPF output channel
>
>
> Hardware Events
>
> * `cpu-cycles` (alias: cycles, default: 1000000); CPU clock cycles
>
> * `instructions` (default: 1000000); CPU instructions
>
> * `cache-references` (default: 1000000); CPU last level cache references
>
> * `cache-misses` (alias: branches, default: 100000); Branch instructions
>
> * `bus-cycles` (default: 100000); Bus cycles
>
> * `frontend-stalls` (default: 1000000); Processor frontend stalls (e.g. instruction fetches)
>
> * `backend-stalls` (default: 1000000); Processor backend stalls (e.g. data loads/stores)
>
> * `ref-cycles` (default: 1000000); CPU reference cycles (e.g. unscaled by turbo)

#### profile and interval

* these are timer based events: `profile:hz:rate`, `profile:s:rate`, `profile:ms:rate`, `profile:us:rate`, `interval:s:rate`, `interval:ms:rate`


### Flow Control

> * 3 type tests in bpftrace: filters, ternary, if statements
>
> * based on boolean expressions `==, !=, >, <, >=, <=, &&, ||`

* Filter: `probe /filter/ { action;s }`

* Ternary Operators: `test ? true_stmt : false_stmt`

* If Statements: `if (test) { true_stmt }`, `if (test) { true_stmt } else { false_stmt }`

> currently `else if { .. }` statements are not supported

* Unrolled Loops: avoids infinite loops; supports `unroll (count) { statements }`

> * count is an integer literal (constant) with max 20
>
> * Linux 5.3 kernel includes support for BPF bounded loops


### Operators

* assignment `=`; arithmetic `+, -, *, /`; auto-increment/decrement `++, --`

* binary and/or/xor/not `&, |, ^, !`

* shift left/right `<<, >>`

* compound operator `+=, -=, *=,/=, %=, &=, ^=, <<=, >>=`


### Variables

> Process Id `pid`, Thread Id `tid`, User Id `uid`, User name `username`, Processor Id `cpu`, Process name `comm`,
>
> Timestamp (nanosec) `nsecs`, Timestamp (nanosecs) since bpftrace init `elapsed`, Kernel Task Struct as uint64 `curtask`,
>
> Kernel Stacktrace `kstack`, User Stacktrace `ustack`, Traced Function `func`, Probe Name `probe`, Cgroup Id `cgroup`,
>
> Some probe type arg `arg0, ..argN` and other arg as `args`, Return Value for some probe types `retval`, Positional args `$1,..$N`

#### Built-ins: pid, comm, uid

* printing who's calling `setuid()` syscall

```
% sudo bpftrace -e 't:syscalls:sys_enter_setuid {printf("setuid by PID: %d (%s),\t UID %d\n", pid, comm, uid); }'
```

* can trace return value by sys_enter_exit; where '-1' is failure

```
% sudo bpftrace -e 't:syscalls:sys_exit_setuid {printf("setuid by PID %d (%s): UID %d\n", pid, comm, args->ret); }'
```

#### Built-ins: kstack, ustack

* kstack/ustack return kernel/user level stack traces as multi-line string, return upto 127 frames

```
% sudo bpftrace -e 't:block:block_rq_insert {printf("Block I/O by %s\n", kstack); }'
Attaching 1 probe...
Block I/O by
        dd_insert_requests+468
        dd_insert_requests+468
        blk_mq_sched_insert_requests+98
...
```

* stack built-ins can also be used as keys in map, freq to be counted

```
% sudo bpftrace -e 't:block:block_rq_insert {@[ustack] = count(); }'
Attaching 1 probe... ^C
@[ fsync+59 ]: 6
@[]: 180
```

#### Built-ins: Positional Parameters

* passed to program on cmd-line, accessible at position as `$1`; usage like `watchconn.bt 101` for passing pid 101

```
% sudo bpftrace -e 'BEGIN {printf("Hey %s!\n", str($1)); }' BPF
Attaching 1 probe...
Hey BPF!
^C
```

#### Scratch & Map

* Scratch like `$duration_us` used for temporary eval in [chapter-05-measure-vfs-read-time.bt](chapter-05-measure-vfs-read-time.bt)

* Map like `@n`, `@n[k]`, `@n[k1 [,k2 [,..]]]`; for storage using hash tables for consistent key-val types


### Functions

> Print formatted `printf(char *fmt [, ...])`, Print formatted time `time(char *fmt)`, Print space joined string array `join(char *arr[])`,
>
> Return string from pointer with optional length `str(char *s [, int len])`,
>
> Return kernel/user stack to limit frames deep `kstack(int l)` & `ustack(int l)`,
>
> Resolve kernel/userspace address to return string symbol using `ksym(void *p)` & `usym(void *p)` resp.,
>
> Resolve kernel/userspace symbol to an address using `kaddr(char *name)` & `uaddr(char *name)` resp.,
>
> Returns value stored in named register `reg(char *name)`, Returns string for IP `ntop([int af,] int addr)`,
>
> Prints contents of file `cat(char *file)`, Exits bpftrace `exit()`; `cgroupid(char *path)` to resolve Cgroup Id

#### `printf()`

* similar to C syntax; allows escape sequences; field descriptions `%[-][width]type` where `-` makes left-justified (default: right)

> * uint `%u`, int `%d`, ulong `%lu`, long `%ld`, unsigned long long `%llu`, long long `%lld`, ushort `%hu`, short `%hd`
>
> * hexadecimal: uint `%x`, ulong `%lx`, ulong long `%llx`
>
> * char `%c`, string `%s`

* example to print 16-char wide `comm` & 6-char wide left-justified `pid` via `printf("%16s %-6d\n", comm, pid);`

#### `join()`

* e.g. attempted execution of commands with their args using `sudo bpftrace -e 't:syscalls:sys_enter_execve { join(args->argv); }'`

* e.g. checking if execution attempt succeeded `% sudo bpftrace -e 't:syscalls:sys_exit_execve { printf("%s %-d\n", comm, args->ret); }'`

> a failed attempt means invocation of a command failed, not necessarily for failure faced due to args within the command

#### `str()`

* default string limit is 64bytes; tunable via `BPFTRACE_STRLEN`

* max allowed 200 bytes until stored in BPF stack; will increase once moved to BPF maps

#### `kstack()` and `ustack()`

* similar to kstack/ustack, with accepting limit: `kstack(limit)`, `kstack(mode[, limit])`, `ustack(limit)`, `ustack(mode[, limit])`

* show top3 kernel frames that led to creating block I/O; max limit is 1024 frames

```
% sudo bpftrace -e 't:block:block_rq_insert { @[kstack(3), comm] = count(); }'
Attaching 1 probe... ^C
@[
    dd_insert_requests+468
    dd_insert_requests+468
    blk_mq_sched_insert_requests+98
, kworker/u16:1]: 1
...
```

> mode arg allows stack output to be formatted differently; supported default `bpftrace` & `perf`

```
% sudo bpftrace -e 'k:do_nanosleep { printf("%s", ustack(perf)); }'
Attaching 1 probe...
	7fe6f239ba95 __GI___clock_nanosleep+117 (/usr/lib/libc-2.33.so)
...
```

#### `ksym()` and `usym()`

* e.g. a function pointer arg as `@[args->function] = count() ;` can display names if used as `@[ksym(args->function)] = count() ;`

* `usym` relies on symbol table in binaries for symbol lookup

#### `kaddr()` and `uaddr()`

* can look for a user-space symbol say `ps1_prompt` when a function is called say `bash:readline` by dereferencing as `sudo bpftrace -e 'uprobe:bash:readline { printf("%s\n", str(*uaddr("ps1_prompt"))); }'`

#### `kptr(void *p)` and `uptr(void *p)`

* generally address space of a pointer gets inferred; but there are some corener cases as kernel func dealing with userspace pointers

```
% sudo bpftrace -e 'k:do_sys_open { printf("%s\n", str(uptr(arg1))); }'  ## usage
```


#### `system()` and `signal()` and `cat()`

* executes command at shell, syntax `system(char *fmt [, args...])` and require `--unsafe` option

> example, calling `ps` to print details on PID calling `nanosleep()`

```
sudo bpftrace --unsafe -e 't:syscalls:sys_enter_nanosleep { system("ps -p %d\n", pid); }'
```

* if used for a frequent event, it will load up more processes per event onto CPU causing overload

* `signal` requires unsafe as well and can send a specified signal to current task `sudo bpftrace -e 'k:f { signal("KILL"); }'`

* `cat` prints file content as

```
% sudo bpftrace -e 't:syscalls:sys_enter_sendmsg { printf("%s => ", comm);
                                                   cat("/proc/%d/cmdline", pid);
                                                   print("\n"); }'
Attaching 1 probe...
systemd-timesyn => /usr/lib/systemd/systemd-timesyncd
systemd-udevd => /usr/lib/systemd/systemd-udevd
...
```

#### `exit()`

```
sudo bpftrace -e 't:syscalls:sys_enter_read { @reads = count() } interval:s:5 { exit(); }'
```


### Map Functions

> Count occurences `count()`; Sum the value `sum(int n)`; Averages the value `avg(int n)`;
>
> Minimum value `min(int n)`; Maximum value `max(int n)`; Print map with optional limits `print(@m [,top [,div]])`
>
> Return count, avg & total `stats(int n)`; Print pow-2 histogram `hist(int n)`; Print linear histogram `lhist(int n, min, max, step)`;
>
> Delete map k/v pair `delete(@m[k])`; Delete all map keys `clear(@m)`; Sets all map values to zero `zero(@m)`

* examples

```
% sudo bpftrace -e 't:block:* { @[probe] = count() } interval:s:5 { exit(); }'
Attaching 19 probes...
@[tracepoint:block:block_plug]: 1
@[tracepoint:block:block_unplug]: 1
...
```

* e.g. print per-interval rate

```
% sudo bpftrace -e 't:block:block_rq_* { @[probe] = count() } interval:s:5 { print(@); print("\n"); clear(@); }'
Attaching 7 probes...
@[tracepoint:block:block_rq_issue]: 5
@[tracepoint:block:block_rq_complete]: 5

@[tracepoint:block:block_rq_insert]: 1
@[tracepoint:block:block_rq_issue]: 8
@[tracepoint:block:block_rq_complete]: 9
...
```

> basic functionality can also be done via `perf stat`

* total bytes read `% sudo bpftrace -e 't:syscalls:sys_exit_read /args->ret > 0/ { @bytes = sum(args->ret); }'`

* error counts are better with freq count of error codes

```
% sudo bpftrace -e 't:syscalls:sys_exit_read /args->ret < 0/ { @[- args->ret] = count(); }'
Attaching 1 probe...^C
@[11]: 9                          ### Error Code: 11 (EAGAIN) is for Try Again in read()
```

* printing time in microsecond using `div`

```
% sudo bpftrace -e 'k:vfs_read { @start[tid] = nsecs; }
                    kr:vfs_read /@start[tid]/ { @us[comm] = sum(nsecs - @start[tid]); delete(@start[tid]); }
                    END { print(@us, 0, 1000); clear(@us); clear(@start); }'
Attaching 3 probes...^C
@us[Monitor thread]: 2
@us[systemd-journal]: 5
@us[ls]: 12
@us[git]: 257
@us[tail]: 14356
...
```


### Future Work

* experimental in Linux 5.3; C style `while` loops as

```
% sudo bpftrace -e 'i:ms:100 { $i = 0; while($i <= 10) {printf("%d ", $i); $i++;} exit();}'
Attaching 1 probe...
0 1 2 3 4 5 6 7 8 9 10
```

* `ply` BPF front-end provides high-level interface without LLVM/Clang depenedency (but struct navigation & including header files not possible)


### Internals

* bpftrace language defined by lex & yacc files; output an AST

* Tracepoint & Clang parsers process structs; Semantic Analyzer checks use of language

* AST nodes converted to LLVM IR, finally compiled to BPF bytecode


### Debugging

* `printf()` could be used to leave breadcrumbs in run

* `-d` prints AST & LLVM IR; `-v` prints BPF bytecode


> [Next Chapter 6. CPUs | Part.II: Using BPF Tools](./chapter-06.md)

---
