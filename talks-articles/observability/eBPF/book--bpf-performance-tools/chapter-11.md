
## Chapter.11 Security

### Background

* BPF allows insight for processes, network connections, system privileges, permission errors, kernel/use fn with args. Checking uncommon syscalls during a runtime.

* Policy engines like `seccomp` can execute BPF to make policy decisions. `Cillium` uses XDP, cgroup & tc based hooks to secure & manage n/w. `bpfilter` a PoC f/w. `Landlock` is a BPF security module providing kernel access.

* BPF programs pass through a verifier. Unprivileged users can use BPF for socket filters only.

* `sysctl -a | grep bpf` to check BPF configurables. `kernel.unprivileged_bpf_disabled`, `net.core.bpf_jit_enable` are key to turn on.


### BPF Tools

```
         ,--------------------------------------,
bashread>| App                                  |    (tools from BCC & bpftrace)
-line    |--------------------------------------|  setuids
         | SysCall Interface                    |<-opensnoop, eperm, shellsnoop,
       |\|------------,-----------,-------------|<-execsnoop, elfsnoop, modsnoop
       | |   VFS      | Sockets   |             |
       | |------------:-----------:Scheduler    |
       | | FileSystem | TCP/UDP <-|-------------|--tcpconnect, tcpaccept, tcpreset
       | |------------:-----------:-------------|  udpconnect
capable> |Volume Mgr  |  IP       |             |
       | |------------:-----------: Virtual Mem |
       | |Block Device| Ethernet  |             |
       | |------------'-----------'-------------|
       | | Device Drivers                       |<-ttysnoop
       |/'--------------------------------------'
```

* `execsnoop` trace new processes. `elfsnoop` trace binary file execution of ELF. `modsnoop` list kernel module loads.

* `bashreadline` trace activity at bash shell. Code for similar

```
#!bpftrace

BEGIN {
    printf("Tracing bash commands... Hit Ctrl-C to end.\n");
    printf("%-9s %-6s %s\n", "TIME", "PID", "COMMAND");
}

uretprobe:/bin/bash:readline {
    time("%H:%M:%S  ");
    printf("%-6d %s\n", pid, str(retval));
}
```

* `shellsnoop [options] $PID` mirrors output of another shell. `-r` emit replay script & `-s` avoids subcommands. Similar code

```
#!bpftrace

BEGIN /$1 == 0/ {
    printf("USAGE: shellsnoop.bt PID\n");
    exit();
}

tracepoint:sched:sched_process_fork
/args->parent_pid == $1 || @descendent[args->parent_pid]/ {
    @descendent[args->child_pid] = 1;
}

tracepoint:syscalls:sys_enter_write
/(pid == $1 || @descendent[pid]) && (args->fd == 1 || args->fd == 2)/ {
    printf("%s", str(args->buf, args->count));
}
```

* `ttysnoop [opts] $DEVICE_ID` to mirror tty/pts device.

* `opensnoop` monitoring file usage. `eperm` tracing syscalls for permission failures.

* `tcpconnect`, `tcpaccept`, `tcpreset` for suspicious n/w activity.

* `capable` to create whitelists of capabilities like `CAP_CHOWN, CAP_KILL, etc.`.


### BPF One-liners

* `funccount -p 1234 'security_*'` for security audit events.

* Tracing start of Pluggable Auth Module `trace 'pam:pam_start "%s: %s", arg1, arg2'`.

* Trace kernel module loads `bpftrace -e 't:module:module_load { printf("load: %s\n", str(args->name)); }'`.

---
