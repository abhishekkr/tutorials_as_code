
## Chapter.13 Applications

> MySQL used for sample. It involes cpu, mem,, fs, disk i/o & n/w insights. App context, threads management, signals, locks & sleeps.

### Background

* Apps have different thread management: Service Thread Pool, CPU Thread Pool, Event Worker Thread (needing locks), SEDA (Staged Event-Driven Arch).

* Linux apps generally use locks (mutex, r/w, spin locks) via `libpthread` lib.

* Apps may have sleep in logic flow.

* Sample: MySQL server. Uses service thread pool, disk bound for heavy data or cpu bound for small sets. Written in C++ has embedded USDT probes for queries, commands, filesort, inserts, etc.

> * USDT probes: `connection__start`, `connection__done`, `command__start`, `command__done`, `query__start`, `query__done`, `filesort__start`, `filesort__done`, `net__write__start`, `net__write__done`. More details at **mysqld DTrace Probe Reference**.
> Only available if compiled with `-DENABLE_DTRACE=1`.

* BPF to answer app requests, latency, on-CPU, waiting & context switch, kernel resource usage.


### BPF Tools

```
        |' ,--------------------------,
        |  |           ,______________|<-- mysqld_slower, mysqld_clat
        |  | App       |Runtimes      |    (tools from BCC & bpftrace)
        |  |     ,--------------------|
        |  |     |  System Libs       |<--threadsnoop, pmlock, pmheld
        |  |--------------------------|
 profile|  | SysCall Interface        |<--syscount killsnoop, signals
        |  |----------,---------------|<--execsnoop, naptime, ioprofile
        |  | Rest of  | Scheduler     |<--threaded
        |  | Kernel   |               |
        |  |----------:---------------|<--offcputime, offcpuhist
        |  | Device Drivers           |
       ,|  '--------------------------'
```

> * CPU analysis: `profile`, `threaded` (pthread lib), `syscount`
> * Off-CPU: `offcputime`, `offcpuhist`, `ioprofile`
> * App Context: `mysqld_slower` (queries slower than threshold), `mysqld_clat`
> * Thread execution: `execsnoop`, `threadsnoop`, `threaded`
> * Lock analysis: `pmlock`, `pmheld` (libpthread mutex lock latencies)
> * Signals: `signals`, `killsnoop`
> * Sleep analysis: `naptime` (tracing syscall)

```
profile -p $(pgrep mysqld) -f 30 > mysql-out.profile01.txt

flamegraph.pl --width=800 --title="CPU Flame Graph" < mysql-out.profile01.txt \
    > mysql-out.profile01.svg

offcputime -f -p $(pgrep mysqld) 10 > mysql-out.offcputime01.txt
flamegraph.pl --width=800 --color=io --title="Off-CPU Time Flame Graph" \
    --countname=us < mysql-out.offcputime01.txt > mysql-out.offcputime01.svg
```


### BPF One-liners

* Mutex lock fn in a sec `funccount -d 1 '/lib/x86_64-linux-gnu/libpthread.so.0:pthread_mutex_*lock'`

* LLC cache misses `bpftrace -e 'hardware:cache-misses: { @[comm] = count(); }'`.

---
