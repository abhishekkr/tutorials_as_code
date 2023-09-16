
## Chapter.15 Containers

### Background

* Common concern of container perf is possible resonance from noisy neighbor. `cgroups` may impose additional limits.

* BPF to check run-queue latency per container, scheduler switching on same cpu between containers, cpu/disk soft limits met.

* BPF on host gets aggregated container traces. Containers use a PID namespace, can be used to differentiate.

* If container uses UTS namespace & sets nodename as container name (like in K8s, docket); then nodename can be used in BPF to filter.

* `kubectl-trace` is K8s scheduler to run bpftrace programs across cluster. As `kubectl trace run -e 'k:vfs* /pid == $container_pid/ { @[probe] = count() }' mypod -a`.


### Traditional Tools

* @Host: `systemd-cgtop` (cgroups' top), `kubectl top`, `docker stats`, `/sys/fs/cgroups`, `perf`. @Cotainer, simple system resource commands.


### BPF Tools

* `runqlat --pidnss -m` showing PID namespaces with run queue latency.

> Can get a PID namespace by `ls -lh /proc/$PID/ns/pid`, result would have `'pid:[$NAMESPACE]'`.

* `pidnss` count of CPU switches between containers by detecting PID namespaces.

* `blkthrot` counts cgroup blk controller throttles I/O based on hard limit.

* `overlayfs` traces r/w latency. `ovl_read_iter`, `ovl_write_iter`, `file_operations_t` kernel fn.


### BPF One-liners

* Trace open file for cgroup v2 name `containX`

```
bpftrace -e 't:syscalls:sys_enter_openat
    /cgroup == cgroupid("/sys/fs/cgroup/unified/containX")/ {
    printf("%s\n", str(args->filename)); }'
```

---
