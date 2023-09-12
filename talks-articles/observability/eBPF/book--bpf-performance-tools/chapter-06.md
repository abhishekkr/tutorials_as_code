
# Part.II Using BPF Tools

## Chapter.6 CPUs

### Background

#### CPU Fundamentals

* CPU Modes: `system mode` is privileged state kernel runs in; `user mode` is for user-level applications accessing resources via Kernel requests (explicit like syscalls or implicit like page faults)

> some housekeeping background threads (like kernel routine to balance memory pages on NUMA) can consume significant CPU resources without explicit user-level requests

* CPU Scheduler: the main consumers are threads (a.k.a. tasks) which belong to processes or kernel routines; cpu consumers also include interrupt routines which can be soft(ware) or hard(ware triggered)

> * 3 thread states: `ON-PROC` for running, `RUNNABLE` for awaiting turn, `SLEEP` for blocked on other event.
> * Threads leave CPU either voluntary (blocked on I/O, lock, sleep) or involuntary (exceeded scheduled CPU alloc time, pre-empted by higher priority thread).
> * CPU affinity make Scheduler only migrate Threads to another idle CPU unless their wait time is short & can use warm hardware caches.

* CPU Caches: based on processor model have multiple levels.

> * L1 (smallest) splits into Instruction & Data caches.
> * End with LLC (Last Level Cache) which is slower and bigger. On a 3-level cache, it'll be L3.
> * MMU (Mem Mgmt Unit) have there own cache, TLB (Translation Lookaside Buffer).

* BPF Capabilities: In addition to CPU use by each proc, context switch rates & run-Q lenghts.. following details are available

> * New proc created, their lifespan. Threads CPU-alloc time per wakup & time in Q.
> * Why is system time high; are syscall causing issues?
> * Max run-Q length; are they balanced across cores.
> * What soft & hard IRQs are consuming CPUs. Why are threads leaving CPU, for how long.
> * CPU idleness. LLC hit ratio.

> By collecting following {events: source}: `Kernel Fn: kprobes, kretprobes`, `App Calls: uprobes, uretprobes`,
> `SysCalls: syscall tracepoint`, `Soft IRQs: irq:softirq* tracepoint`, `Hard IRQs: irq:irq_handler* tracepoint`,
> `WorkQueue: workqueue tracepoints`, `Timed Sampling: PMC`, `CPU Power: power tracepoints`, `CPU Cycles: PMCs`.
>
> Need to consider overhead as events like Context Switch would be on a huge scale. So filtering frequent events & time-sampling shall be used.

#### Sample Strategy

* Check CPU utilization (e.g. `mpstat`) & all load balanced overall cores.

* Confirm workload is CPU-bound. Check if load is system-wide or single CPU. Check high run-queue latency (e.g. `BCC runqlat`).

* Quantify % usage broken down by proc, cpu-mode & cpu-id. Looking for outliers. For high time/freq using `perf`, `bpftrace one-liners`, `BCC sysstat`. Profile & flame graph.

* Fetch more context on outliers. E.g. is busy with `stat() on files` use `BCC statssnoop` or `kprobe tracepoints`. `uprobes` to identify what requests are keeping it busy. `BCC hardirqs` for H/w interruprs. Measure CPU instruction/cycle (IPC) using `PMCs` or low cache-hit using `BCC llcstat`.


### Traditional Tools

#### Kernel Stats (often exposed under `/proc`)

* `uptime` with last 3 fields as {1,5,15}-min load averages. These are exponentially damped. Divided by core count give CPU saturation, although impacted by other disk/mem factors. BPF-vased tools like `offcputime` shows if load is CPU or uninterruptable-time based.

* `top` shows CPU consumings procs with summary header. Data points can skip the refresh, `pidstat` can be used to print rolling output. Alternative `tiptop` sources PMCs, `atop` process events to display short-lived procs, BCC's `biotop` & `tcptop` use BPF.

* `mpstat` to examine per-CPU metric. Time is broken down to hard interrupts `%irq` & soft interrupts `%soft`. Further details using BPF's `hardirqs` & `softirqa` tools.

#### H/w Stats

* `perf` linux profiler as `perf stat -d $CMD`. Shows extended set with `-d`, counts events with `-e`. Detailed PMC list for h/w can be viewed with `perf list`, showing event alias that can be counted. `-I 1000` would print per-second (1K millisec) event rates.

* `tlbstat` counts & sumarizes TLB-related PMCs. Showing `K_CYCLES: CPU cycles 1K lot`, `K_INSTR: CPU instruction 1K lot`, `IPC: Instruction per cycle`, `DTLB_WALKS: Data TLB Walks`, `ITLB_WALKS: Instr..`, `K_DTLBCYC: 1K lot cycles with at least 1 PageMissHandler active with DTLB`, `K_ITLBCYC: ..for Instr..`, `DTLB%: Data TLB active cycles as ratio of total cycles`, `ITLB%: ..Instrs..`. High `DTLB%` & `ITLB%` show need to TLB tuning.

> H/w sample recording..
> * `sudo perf record -e L1-dcache-load-misses:u -c 50000 -a -g -- find .` makes PMC event cause interrupt to capture event states at given rate once every event period `-c 50000`; `-g` enabling call graph. Records to a file `perf.data` available to review using `perf report`. [usage](https://man.archlinux.org/man/perf-record.1.en).

> Timed Sampling..
> * `sudo perf record -F 99 -a -g -- find .` samples all CPUs at 99Hz (samples/sec-per-CPU) for command runtime. `99` is chosed to avoid **lockstep sampling** with other routines. Each sample can be dumped using `perf script`.
> * Flame Graphs are well suited to CPU analysis.

```
git clone https://github.com/brendangregg/FlameGraph && cd FlameGraph
perf record -F 49 -ag -- sleep 30
perf script --header | ./stackcollapse-perf.pl | ./flamegraph.pl > flame1.svg
```

> * `perf` uses PMC-based NMI (non-maskable interupt) for timed sampling. Many Cloud Instances don't have PMCs enabled. Can check using `dmesg | grep PMU`. There `perf` fallbacks to hrtimer-based s/w interrupt. The s/w interrupt might be unable to interrupt certain kernel paths, thus missing those samples.

> E.g. Overall usage might be high instead of a proc visible in `top`. Could be several short-lived processes.
> Counting `sched_process_exec` tracepoint via `perf` works. Can also use BCC's `execsnoop`.

```
sudo perf record -e sched:sched_process_exec -I 1000
sudo perf script
```

> * `perf sched` uses dump-and-post process approach to analyze scheduler behavior; providing report on CPU runtime per wakeup, latency, more

```
sudo perf sched record -- sleep 5
sudo perf sched timehist
```

> Using BPF tools to do in-kernel summaries for these via `runqlat`, `runqlen` tools.

* `Ftrace` (heavily used in BGregg's perf-tools repo), has many useful tracing abilities.


### BPF Tools

```
       |' ,--------------------------,
       |  | App       |Runtimes      |    (tools from BCC & bpftrace)
       |  |     ,--------------------|
argdist|  |     |  System Libs       |
  trace|  |--------------------------|
funccount | SysCall Interface        |<--syscount
profile|  |----------,---------------|<--execsnoop, exitsnoop
       |  | Rest of  | Scheduler     |<--cpudist, runqlat, runqlen, offcputime
       |  |          |               |   cpufreq, runqslower
       |  | Kernel   |    [Interrupts]<--hardirqs, softirqs, smpcalls
       |  |----------:---------------|
       |  | Device Drivers           |    [CPUs]<--llcstat, cpufreq
      ,|  '--------------------------'
```

* `execsnoop` traces `execve` syscall showing args & return val. Catches new proc `fork/clone->exec` & proc that `re-exec`. Core functionality as

```
#!bpftrace
BEGIN {
    printf("%-10s %-5s %s\n", "TIME(ms)", "PID", "ARGS");
}
tracepoint:syscalls:sys_enter_execve {
    printf("%-10u %-5d ", elapsed/1000000, pid);
    join(args->argv);
}
```

* `exitsnoop` a BCC tool tracing proc exits, showing age & reason.

* `runqlat` to measure CPU scheduler latency, to check CPU saturation.

```
#!bpftrace
#include <uapi/linux/ptrace.h>
#include <linux/sched.h>
#include <linux/nsproxy.h>
#include <linux/pid_namespace.h>
BEGIN {
    printf("Tracing CPU sched.. Ctrl+C to end.\n");
}
tracepoint:sched:sched_wakeup,
tracepoint:sched:sched_wakeup_new {
    @qtime[args->pid] = nsecs;
}
tracepoint:sched:sched_switch {
    if(args ->prev_state == TASK_RUNNING) {
        @qtime[args->prev_pid] = nsecs;
    }
    $ns = @qtime[args->next_pid];
    if($ns) {
        @usecs = hist((nsecs - $ns) / 1000);
    }
    delete(@qtime[args->next_pid]);
}
END {
    clear(@qtime);
}
```

* `runqlen`, below similar sample code to fetch details at 99Hz

```
#!bpftrace
#include <linux/sched.h>

struct cfs_rq_partial {
  struct load_weight load;
  unsigned long runnable_weight;
  unsigned int nr_running;
}

BEGIN {
    printf("Sampling run queue length at 99Hz.. Ctrl+C to end\n");
}

profile:hz:99 {
  $task = (struct task_struct *)curtask;
  $my_q = (struct cfs_rq_partial *)$task->se.cfs_rq;
  $len = $my_q->nr_running;
  $len = $len > 0 ? $len - 1 : 0;  // subtract currently running tasks
  @runqlen = lhist($len, 0, 100, 1);
}
```

* `runqslower` fetch RQ latencies breaking configurable threshold.

* `cpudist` shows on-CPU time for each thread wakeup.

* `cpufreq` samples CPU freq, determine what clockspeed app is running at. CPU scaling governors are power-schemeswith pseudo-governors like `powersave` & `performance` having different impact on app perf. Src as tracking freq from 0-5000 MHz in 200MHz steps..

```
#!bpftrace

BEGIN {
    printf("Sampling CPU freq system-wide & by process. Ctrl-C to end.\n");
}

tracepoint:power:cpu_frequency {
    @curfreq[cpu] = args->state;
}

profile:hz:100 /@curfreq[cpu]/ {
    @system_mhz = lhist(@curfreq[cpu] / 1000, 0, 5000, 200);
    if (pid) {
        @process_mhz[comm] = lhist(@curfreq[cpu] / 1000, 0, 5000, 200);
    }
}

END {
    clear(@curfreq);
}
```

* BCC's `profile` sample stacktraces & emits freq count, default at 49Hz. Counts in Kernel space, thus more efficient.

```
bpftrace -e 'profile:hz:49 /pid/ { @samples[ustack, kstack, comm] = count(); }'
```

* BCC's `offcputime` gists threads time spent off-CPU, showing stacktraces. A counterpart to `profile`. Instuments context-switches & records time alongwith stacktraces.

* BCC's `syscount` counting syscalls (be overhead aware). Some cases might favor using `strace` for insight into syscall usage.

```
bpftrace -e 't:syscalls:sys_enter_* { @[probe] = count(); }'
```

* Details on syscall causing load by BCC's `argdist` (get args & ret-val, also using `tplist`) & `trace` (per event output, suited for less frequent syscalls) can examine events.

```
tplist -v syscalls:sys_enter_read
argdist -H 't:syscalls:sys_enter_read():int:args->count'
argdist -H 't:syscalls:sys_exit_read():int:args->ret'

bpftrace -e 't:syscalls:sys_enter_read { @ = hist(args->count); }'
bpftrace -e 't:syscalls:sys_exit_read { @ = hist(args->ret); }'

## for Histogram
bpftrace -e 't:syscalls:sys_exit_read /args->ret < 0/ { @ = lhist(- args->ret, 0, 100, 1); }'
```

* BCC's `funccount` shows which func called & how often

```
bpftrace -e 'k:tcp_* { @[probe] = count(); }'
```

* SMP calls are way for one CPU ton run Fn on another CPUs; expensive over large multi-processors. `smpcalls` traces SMP call fn.

* BCC's `llcstat` uses PMCs to sample LastLevelCache hit/miss rates.

* `cpuwalk` sample procs in CPU with histogram view of CPU balance. `cpuunclaimed` tries to gather idleness data, sign of scheduler misconfig. `loads` fetch load averages. `vltrace` for syscall chracterization on CPU usage.


### BPF One-Liners

* New procs with args: `execsnoop`

* Syscall count by procs: `syscount -p`. By syscall name: `syscount`.

* Sample user-level stack at 49Hz for PID 100: `profile -F 49 -U -p 100`. Also `bpftrace -e 'profile:hz:49 /pid == 100/ { @[ustack] = count(); }'`.

* Call details `bpftrace -e 'tracepoint:syscalls:sys_enter_execve { printf("%s -> %s\n", comm, str(args->filename)); }'`.

---
