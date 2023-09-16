
## Chapter.16 Hypervisors

### Background

* Hypervisor Patterns

> * Xen Style: a native (bare-metal) hypervisor running directly on processors. Create domains for VMs.
> * KVM style: hypervisor run/administered by Host OS kernel. H/w access via Kernel Modules.

* Xen style, runs I/O proxy (like QEMU) in domain0. KVM style on host OS.

* `AMD-V` & `Intel VT-x` extensions bring processor virtualization support.

* `Paravirtualization` has guest OS aware of virtual machine, making hypercalls to hypervisor for efficiency. Xen batches these calls into multicall.

> Now, Xen VMs boot in H/w VM (HVM), then use PV.. hence PVHVM.

* @Guests: BPF to check virtualized h/w perf; hypercall latency; stolen CPU time; hypervisor interrupt callbacks.

* @Hosts: Instrument VM exits; I/O proxy workload & latency; prior tools for perf.


### Traditional Tools

* Guests sometimes have tracepoints for Xen Hypercalls.

* Hosts have top/trace tools for hypervisor provided, like `xl top`, `xentrace`.


### Guest BPF Tools

* Xen Hypercalls using `funccount, trace, argdist, stackcount`; get PV status via `dmesg | grep Hypervisor`. Like `funccount 't:xen:*'`.

```
argdist -C 't:xen:xen_mc_flush():int:args->mcidx'  ## counting Hypercalls

stackcount 't:xen:xen_mc_issue'  ## hypercall stacks

funclatency xen_mc_flush  ## hypercall latency
```

* `xenhyper`, bpftrace tool counts hyperalls via `xen:xen_mc_entry` tp.

* When Xen calls guest, `/proc/interrupts` get grep-able `HYP` Xen callback entries. Trace via `bpftrace -e 'kprobe:xen_evtchn_do_upcall { @[comm] = count(); }'`.

* `cpustolen`, bpftrace tool list stolen CPU time dist. Cab also include time in VMM on behalf of guest. Similar code

```
#!bpftrace

BEGIN {
    printf("Tracing stolen CPU time. Ctrl-C to end.\n");
}

kretprobe:xen_steal_clock, kretprobe:kvm_steal_clock {
    if (@last[cpu] > 0) {
        @stolen_us = hist((retval - @last[cpu]) / 1000);
    }
    @last[cpu] = retval;
}

END {
    clear(@last);
}
```


### Host BPF Tools

* `kvmexits`, bpftrace tool to show guest exit time dist. Works by tracing `kvm:kvm_exit` & `kvm:kvm_entry`.

---
