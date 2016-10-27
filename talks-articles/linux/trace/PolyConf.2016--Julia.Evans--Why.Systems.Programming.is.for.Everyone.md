
## Why Systems Programming is for Everyone

### Wizard School OR Love OS

Know your System Calls like
> Open/Write a File (open,write)
> Start a Program (execve)
> Receiving data (recvfrom)
> Change a file's permissions (chmod)

---

### Case of Mystery Config File

##### STRACE ~ Tracing System Calls

```
strace $MYPROGRAM
```

* checking up on all `open` calls

```
strace -e open $MYPROGRAM
```

so, to see what config file gets opened when

```
strace -e open bash

...
open("/dev/tty", O_RDWR|O_NONBLOCK)     = 3
open("/home/abhishekkr/.bashrc", O_RDONLY) = 3
open("/home/abhishekkr/.profile", O_RDONLY) = 3
...
```


##### OpenSnoop

[https://github.com/iovisor/bcc](https://github.com/iovisor/bcc)

> works on OSX, linux

Uses Linux eBPF/bcc. By Brendan Gregg.

---

### Case of Polish Website

##### Wireshark

got awesome filtering, so use it

```
frame contains "GET"
```


#### tcpdump

faster, BPF (berkeley packet filter)

```
src ip 192.168.0.1 or dst ip 192.168.0.1
```

> **ngrep, mitmproxy**

---

### Case of Slow Program

may be cuz
> CPU time
> too many writes
> waiting on a slow response

##### time

simple solutions are best, get the time break

```
time $MYPROGRAM
```


##### `/proc/$PID/stack`

```
pgrep -f $MYPROGRAM

sudo cat /proc/$MYPID/stack
```


##### dstat

> to generate system resource statistics, can be run while running program

```
dstat -d
```


##### perf

```
perf top
```

* work with JVM, [github.com/jrudolph/perf-map-agent] a java agent to generate method mappings to use with linux `perf` tool

---

write a kernel
write a network stack

---
---
