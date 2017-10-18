

## Performant Security Hardening (KVM and x86)

> by Steve Rutherford (from Google Virtualization Security Team)

* Threat Model: Untrusted Users with Code Execution in VM.

* Goal: Reduce KVM's Guest accessible attack surface (amount or privilege).


#### Why Put Code in Userspace

* lower privilege: syscall boundary between userspace and kernel

* exploit mitigation and sandboxing: more easily deployed in userspace (ASLR, AppArmor, seccomp-bpf)


#### Why NOT Put Code in Userspace

* performance, userspace devices require KVM exits with higher latency and lower throughput


#### What can be moved to Userspace

code that's complex, slow and rarely used; but necessary

* legacy devices; e.g. PIC, PIT, I/O APIC
* edge-case handlers; e.g. Instruction Emulators, MSR handling


#### Split Irqchip

* Irqchip

> KVM uses Irqchip to refer to interrupt controolers i.e. PIC, I/O APIC and APIC for x86.
>
> KVM has support for both userspace and kernel irqchips.
> Kernel irqchips provides a significant perf boost over userspace irqchip.


* Split Irqchip

> Take best of Userspace and Kernel irqchips.
>
> APIC is often used by modern VMs.
> So move PIC and  I/O APIC to userspace and add necessary API to communicate between userspace and in-kernel APIC.


* PIC (Programmable Interrupt Controller)

> Interrupt controoler that maps directly from GSI to interrupt. Can't live without it.
>
> Necessary for real mode interrupts (16-bit) during early boot. Allow legacy devices like RTC and PIT to send interrupts.
>
> Masked early in boot and replaced by I/O APIC.


* I/O APIC

> Global Interrupt Controller; PIC for MultiCore
> Necessary for any non-MSI-supporting (INTx) device (PIT, RTC, ...)


* PIT (Programmable Interval Timer)

> fixed frequency timer with multiple counters
> commonly used to calibrate other timers/counters


*With Split IRQChip, almost all MMIO devices are now in userspace.*
*APIC is an exception, but APICv skips emulation.*

---
---
