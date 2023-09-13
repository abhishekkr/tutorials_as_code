
## eBPF

> Enhanced Berkley Packet Filter or `eBPF` is a bytecode virtual machine in Linux Kernel used to trace kernel functions.
>
> It's not just packet filtering, it's for insight into varying kind of system layer activities.

* [Book Notes: BPF Performance Tools](./book--bpf-performance-tools/README.md)

---

source/references/detailed-reads

* `bpf()` syscall in [Linux 3.18](https://github.com/torvalds/linux/tree/master/tools/lib/bpf)

* [BPF Compiler Collection by IO Visor Project](https://github.com/iovisor/bcc), rich helper function by its [libbpf](https://github.com/iovisor/bcc/blob/master/src/cc/libbpf.c)

* list of pseudo-assembly instructions allowed [struct bpf\_insn prog](https://github.com/torvalds/linux/blob/master/samples/bpf/sock_example.c)

* [introducing gobpf](https://kinvolk.io/blog/2016/11/introducing-gobpf---using-ebpf-from-go/)

---
