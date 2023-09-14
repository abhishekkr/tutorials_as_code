
## Chapter.8 File Systems

> Tracing when app waits on disk I/O, locks or similar.

### Background

* I/O stack

```
 ,---,.____________,-----,.___________,--------,._____Raw_I/O_________,----------,
 |App|:-[Runtime]:-|POSIX|:-[SysLibs]-|SysCalls|:-[VFS]-[FS]-[VolMgr]-|DiskDevice|
 '---'             '-----'            '--------'  Logical I/O~~~~~~~  |SubSystem |
                                                                      '----------'
 * many I/O gets served from FS cache, never makes phys.I/O
 * VFS is generic kernel interface allowing different FS supported via SysCalls
```

* FS Caches

```
 [Page Cache]<---->[   VFS       ]<-,--->[Dir Cache]
 [Page Scanner]    [ext4][xfs][..]  '--->[inode Cache]
                    |,    |,   |,
            [Block Device Interface]<--->[Disks]

 * Page Cache grows to be largest as caches file object, also includes 'dirty' pages
```

* `Read Ahead` detects a sequential read, predicts next and load into page cache. `readahead` syscall.

* `Write Back` flush dirtied buffers to disk by kernel worker threads, not blocking app on slow disk.

* BPF observability for FS requests (count by type); FS read sizes; Sync I/O write volume; File access workload; File causing issues; FS latency distribution; Page Cache (,Dcache & Icache) hit/miss; more.

* Instrumentable I/O types:

> * App & Lib I/O: `uprobes`
> * SysCall I/O: `syscalls tracepoints`
> * FS I/O: `ext4 (..) tracepoints, kprobes`
> * Cache hit (reads, write-back) & miss (reads, write-through): `kprobes`
> * Page Cache write-back: `writeback tracepoints`
> * Physical Disk I/O: `block tracepoints`, `kprobes`. Raw I/O: `kprobes`.

* `VFS` is used heavily by many n/w I/O paths, irresponsible tracing can cause Overload. For Physical I/O tracing, check activity via `iostat` to gauge overload.


### Strategy

* Check mounted FS, `df` or `mount`. Check capacity. Try create some FS workload, `fio`. Use `opensnoop` to see files accessed & `filelife` for short-lived files related issues.

* Use `fileslower` (or `ext4slower`, `btrfsslower`, `zfsslower`) to examine proc & file details for slow I/O.

* Check FS latency distribution; `ext4dist` (or `btrfsdist`, `zfsdist`, etc).

* `cachestat` to check page cache hit ratio over time, if any tuning improves it.

* Comparing `vfsstat` logical I/O rates to physical I.O rates from `iostat`. Grater logical rates indicate effective caching.


### Traditional Tools

* `df` showing filesystems, mount points with their capcity. `mount` showing mount flags as well.

* `strace` grabs FS ops from syscall traces. E.g. `strace cksum -tttT /usr/bin/cksum` uses `-ttt` to print wall timestamps & `-T` for syscall duration. `strace` may slow system down.

* `perf` examples: `perf trace cksum /usr/bin/cksum` uses kprobes to inspect VFS, more efficiently. `perf stat -e 'ext4:*' -a` counting all ext4 calls (depends on what tracepoints are supported by FS).

* `fatrace` special tracer for FANotify (file access); showing path, event, PID, proc name & status. Overload for busy systems. `opensnoop` is efficient alternative.


### BPF Tools

```
 ,--------------------------,
 | App                      |    (tools from BCC & bpftrace)
 |--------------------------|
 | SysCall Interface        |<--opensnoop, statssnoop, syncsnoop, mmapfiles
 |----------,---------------|   scread
 | Rest of  | [VFS        ]-|<--filetop, filelife, fileslower, filetype, fmapfault
 |          |               |   vfssize, vfscount, vfsstat, writesync, fsrwstat
 |          | [FileSystems]-|<--mountsnoop, writeback, ext4dist, xfsslower,..
 | Kernel   | [VolManager ]-|
 |          | [BlockDevice]-|          also bufgrow, dcsnoop, dcstat, cachestat
 |----------:---------------|
 | Device Drivers           |
 '--------------------------'
```

* BCC's `opensnoop -T` traces file opens with `-T` giving timestamps. `-x` shows failure only & `-p $PID` for proc specific. Code for similar

```
#!bpftrace

BEGIN {
    printf("Tracing open syscalls... Hit Ctrl-C to end.\n");
    printf("%-6s %-16s %4s %3s %s\n", "PID", "COMM", "FD", "ERR", "PATH");
}

tracepoint:syscalls:sys_enter_open, tracepoint:syscalls:sys_enter_openat {
    @filename[tid] = args->filename;
}

tracepoint:syscalls:sys_exit_open, tracepoint:syscalls:sys_exit_openat
/@filename[tid]/ {
    $ret = args->ret;
    $fd = $ret > 0 ? $ret : -1;
    $errno = $ret > 0 ? 0 : - $ret;

    printf("%-6d %-16s %4d %3d %s\n", pid, comm, $fd, $errno,
        str(@filename[tid]));
    delete(@filename[tid]);
}

END {
       clear(@filename);
}
```

* `statsnoop` shows file statistics for `syscall.stat`. Supports `-x` & `-p $PID`. Using tracepoints `tracepoint:syscalls:sys_enter_statfs`, `tracepoint:syscalls:sys_enter_statx`, `tracepoint:syscalls:sys_enter_newstat` & `tracepoint:syscalls:sys_enter_newlstat`.

* `syncsnoop` traces `sync` for flush of dirty data to disk. Code for similar

```
#!bpftrace

BEGIN {
    printf("Tracing sync syscalls... Hit Ctrl-C to end.\n");
    printf("%-9s %-6s %-16s %s\n", "TIME", "PID", "COMM", "EVENT");
}

tracepoint:syscalls:sys_enter_sync, tracepoint:syscalls:sys_enter_syncfs,
tracepoint:syscalls:sys_enter_fsync, tracepoint:syscalls:sys_enter_fdatasync,
tracepoint:syscalls:sys_enter_sync_file_range, tracepoint:syscalls:sys_enter_msync {
    time("%H:%M:%S  ");
    printf("%-6d %-16s %s\n", pid, comm, probe);
}
```

* `mmapfiles` traces `mmap`. Code similar to

```
#!bpftrace

#include <linux/mm.h>

kprobe:do_mmap {
  $file = (struct file *)arg0;
  $name = $file->f_path.dentry;
  $dir1 = $name->d_parent;
  $dir2 = $dir1->d_parent;
  @[str($dir2->d_name.name), str($dir1->d_name.name),
    str($name->d_name.name)] = count();
}
```

* `scread` traces syscall `read` (a syscall based view, `filetop` is VFS based view).

* `fmapfault` trace mmap files' page faults. Syscall's r/w variant based tools escaped by file mappings. Similar code

```
#!bpftrace

#include <linux/mm.h>

kprobe:filemap_fault {
    $vf = (struct vm_fault *)arg0;
    $file = $vf->vma->vm_file->f_path.dentry->d_name.name;
    @[comm, str($file)] = count();
}
```

* `filelife` shows AGE of files, allowing check short-lived files. Supports `-p $PID`. Similar code

* `fsrwstat` customize `vfstat` to include FS type.

* `fileslower` traces file r/w slower than threshold. `filetop` for frequently r/w filenames.

* `cachestat` for hit/miss ratio; check if tunings are working.

* `dcstat` shows dir entry cache `dcache` hit/miss stats. `dcsnoop` for tracing dcache lookups.

* `icstat` for Inode cache hit.miss stats. `bufgrow` for insight into buffer cache. `readahead` for efficiency.


### BPF One-liners

* `stackcount -p $PID 't:syscalls:sys_enter_openat'` counting stacks for file open tracepoint.

* File created via `trace 't:syscalls:sys_enter_creat "%s", args->pathname'`.

* Count read `funccount 't:syscalls:sys_enter_*read*'`. Write with `funccount 't:syscalls:sys_enter_*write*'`.

* Read errors count `argdist -C 't:syscalls:sys_exit_read():int:args->ret:args->ret<0'`.

* Reads to device via `stackcount -P read_pages`. Ext4's `stackcount -P ext4_readpages`.

---
