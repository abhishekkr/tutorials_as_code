## Intro to Linux Control Groups (cgroups)
_sysadmincasts_

Managing and Monitor system resources by parititioning
* CPU Time
* System Memory
* Disk Bandwidth
* Network Bandwidth
into groups and assigning tasks to those groups.

---

To assign and isolate an application into a profile.

> Example:
> [ Group.2 SomeApp ]
> Profile
> * 80% CPU
> * 10 GB memory
> * 80% rw blk dev
> * 80% network bw

Can adjust limits on the fly.
Resources can be monitored as cpu, memory, iops/bytes in/out, network bytes.

---

#### Hands-on

* Need package 'libcgroup' installed, it will place bunch of utilities required for the workflow (like cgclassify, cgcreate, cgdelete, cgexec, cgget, cgset, cgsnapshot, lscgroup, lssubsys, cgclear, cgconfigparser, cgrulesengd).

* Generally creates '/cgroup' to mount different cgroups subsystem

* '/etc/cgconfig.conf' provides main ground rules to cgroups for mounting a subsystem

* mount control groups, start cgconfig service
```
service cgconfig start #init.d
systemctl start cgconfig #systemd
```

* Now '/cgroup' will have controllers mounted, 'ls /cgroups' will show.

* Listing Block IO 'ls -lah /cgroup/blkio', shows multiple files prefixed name with blkio showing they belong to this controller. There are 5 common files
> * 'tasks' contain list of process-ids, assigned processes to this cgroup
> * 'cgroup.procs' contain group-ids, useful for multi-threaded applications
> * 'cgroup.event\_control' used to hook-in a notification API for any state monitored
> * 'notify\_on\_release' when all process in cgroup terminate, true||false
> * 'release\_agent' could be the script which gets notified by 'notify\_on\_release'

* to create a new child-group, can use 'cgcreate' or just create directory under required path, the sub-dir i.e. child-group will get automatically populated with similar set of files as parent-group. With exception of 'release\_agent' file, that's just one at root controller.
```
mkdir /cgroup/blkio/testGroup
```

* to list all controller groups and sub-groups
```
lscgroup
```

##### trying throttle read/write for sub-group 'testGroup'

* use 'iotop' to get status of anything reading/writing to disk
```
iotop -o
```

* can use 'dd' to check process inside and outside control group doing disk read/write
```
dd if=/dev/zero of=/tmp/file-abc bs=1M count=3000
```
> before testing run 'sync' to flush any file-buffers to disk,
> then 'free -m' to see if anything cached, drop the cache by 'echo 3 > /proc/sys/vm/drop\_caches' so doesn't interfere in results

* do an 'ls -lah' at device block to get it's major minor number required for throttle instrumentation; if picked 
'/dev/sda' the number columns are '8, 0' which is passed as '8:0'
```
$ ls -lah /dev/sd*
brw-rw---- 1 root disk 8,  0 Jan 16 10:24 /dev/sda
brw-rw---- 1 root disk 8,  1 Jan 16 10:24 /dev/sda1
brw-rw---- 1 root disk 8,  2 Jan 16 10:24 /dev/sda2
brw-rw---- 1 root disk 8,  3 Jan 16 10:24 /dev/sda3
brw-rw---- 1 root disk 8,  4 Jan 16 10:24 /dev/sda4
brw-rw---- 1 root disk 8,  5 Jan 16 10:24 /dev/sda5
brw-rw---- 1 root disk 8, 16 Jan 16 13:48 /dev/sdb
brw-rw---- 1 root disk 8, 17 Jan 16 15:45 /dev/sdb1
brw-rw---- 1 root disk 8, 18 Jan 16 13:48 /dev/sdb2
brw-rw---- 1 root disk 8, 19 Jan 16 13:48 /dev/sdb3
```

* now, byte/second throttle for '/dev/sda' can be set using following command using major:minor number from last enquiry to 5MB(5242880)
```
echo "8:0 5242880" > /cgroup/blkio/testGroup/blkio.throttle.read_bps_device
```

* check normal stats for test command
```
dd if=/tmp/file-abc of=/dev/null
```

* now drop cache and use 'cgexec' to run command in a cgroup, it should be limited to 5MB
```
cgroup -g blkio:/testGroup dd if=/tmp/file-abc of=/dev/null
```

##### throttle memory

* mem-limit.c file to test it, 'gcc mem-limit.c -o mem-limit'
```C
#include <string.h>

int main(void){
  int i;
  char *p;

  printf("starting...\n");

  for(i=0; i<50; ++i){
    p = malloc(1<<20)
    if(p == NULL){  //if not allocated
      printf("Malloc failed at %d MB\n", i)
    }
    memset(p, 0, (1<<20));  //take memory and tell user where we are at
    printf("Allocated %d to %d MB\n", i, i+1);
  }
  printf("Done!\n");
  return 0;
}
```

* check if it works, './mem-limit'

* create a memory controller's sub-group
```
mkdir /cgroup/memory/testGroup
```

* the two property configured are 'memory.limit\_in\_bytes' (limit of memory usage) and 'memory.memsw.limit\_in\_bytes' (limit of memory+Swap usage)

* to limit memory usage we need to push value same as blkio
```
echo 5242880 > memory.limit\_in\_bytes
echo 5242880 > memory.memsw.limit\_in\_bytes
```

* testing it, should get killed; can check error using dmesg
```
cgexec -g memory:/testGroup ./mem-limit

dmesg | tail -n 15
```

##### some more

* to dump running cgroup configuration
```
cgsnapshot -s
```

* can back-it up for next reboot as
```
cgsnapshot -s > /etc/cgconfig.conf
```

* 'cgred' can be used to classify programs in background to cgroup; can define rules to automatically include execution under cgroup without cgexec i.e. using '/etc/cgrules.conf'

---

References:

* [kernel.org Doc cgroups](https://www.kernel.org/doc/Documentation/cgroups/cgroups.txt)
* [kernel.org Doc blkio throttling](https://www.kernel.org/doc/Documentation/cgroups/blkio-controller.txt)
* [kernel.org Doc memory](https://www.kernel.org/doc/Documentation/cgroups/memory.txt)

---
---
