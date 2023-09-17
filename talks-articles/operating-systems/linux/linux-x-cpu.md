
## Linux x CPU

### CPUFreq Governor

> [source](https://www.kernel.org/doc/Documentation/cpu-freq/governors.txt)

* cpufreq governors decide frequency limits (dynamic/static).

* Governors in Kernel:

> * `performance` (static highest between `scaling_min_freq` to `scaling_max_freq`)
> * `powersave` (static lowest between min/max)
> * `userspace` (allows proc with UID:root to set freq making `sysfs` file `scaling_setspeed`)
> * `ondemand` (based on current load, quickly)
> * `conservative` (based on current load, graceful updates)
> * `schedutil` (tries better integration with Kernel scheduler, via `PELT`, Per-Entity Load Estimation)

* New governor must register using `cpufreq_register_governor`.

---
