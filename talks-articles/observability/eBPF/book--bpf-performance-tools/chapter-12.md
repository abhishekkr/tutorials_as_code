
## Chapter.12 Languages

### Background

* For any language to instrument; need to find fn names (symbols) & args; ways to investigate stacktraces.

* Know how gets converted to machine code.

> * Like, since Java code runs on JVM. The encountered code maybe compile C++ JVM, interpreted Java method or JIT-compiled state.
> * For binary crunching compiled languages like Go/C++/etc.; ELF (or platform specific formats) have symbol tables. Fn arg & retvals are stored in registers ad stack offsets. Frame pointer register can be walked for stacktrace.

* Some compilers need to switch-in frame pointer based stack walking like `gcc -fno-omit-frame-pointer ..`.

* JIT happens on the fly, so no pre-built symbol table. Mappings are in JIT runtime.

> `uprobes` are inode-based requiring a file location to work, JIT fn may be private anon mappings. Possible if runtime provides USDT probes.

* Interpreters have symbol table within.

### BPF Tools

```
            /'\,--------------------------,         for c, Java & Bash
             | | App                      |    (tools from BCC & bpftrace)
             | |                          |<--bashfunc, bashfunclat
             | |                 [Runtimes]<--javastat, javacalls, javaflow, javagc,
    trace    | |         [System Libraries]<-jnistacks        javaobjnew, javathreads
    argdist  | |--------------------------|
    funccount| | SysCall Interface        |
   stackcount| |----------,---------------|
    profile  | | Rest of  |  Scheduler    |
    bpftrace | | Kernel   |               |<--offcputime
             | |----------:---------------|
             | | Device Drivers           |
            \,/'--------------------------'
```

#### C

* Kernel-level C has its own symbol table, most Fn can be seen & traced.

* Some C libs provide USDT probes by default, like libc. Use e.g. `bpftrace -l 'usdt:/lib/x86_64-linux-gnu/libc-2.27.so'`.

* `readelf` can bbe used to check if ELF symbol tables is present.

* Statically compiled app might have been stripped of symbols. Could use DWARF debuginfo, or BTF. Debuginfo files if present would have `.debuginfo` extension.

* C One-liners:

> * `funccount '/bin/bash:ls*'` for counting fn call starting with ls.
> * Counting lib calls starting with 'm': `funccount '/lib/x86_64-linux-gnu/libc.so.6:m*'`.
> * Trace Fn & args `trace '/bin/bash:readline "%s", arg1'`. Trace retvals `trace '/bin/bash:readline "%s", retval'`.

#### Java

* JVM runs bytecode; when passed execution threshold it JIT-compile to native instructions. Also profile method execution & optimize methods.

* Comes with many USDT probes; like VM/Thread lifecycle, Class loading, GC, Method compilation/calls, Monitor, App tracking, Object alloc, etc. (available if JVM was compiled with `--enable-dtrace` option).

> * Probes can be checked using `tplist -p $SOME_JAVA_PID`.
> * Run with `-XX:+ExtendedDTraceProbes`.

* Tools: `jnistacks` list JNI consumers; `javastat` list stacktraces; `javathreads` for thread start/stop events; `javacalls` for method calls; `javaflow` shows code flow; `javagc` traces GC & `javaobjnew` counts new alloc. Also available are `profile`, `offcputime` & `stackcount` for general tracing.

* Java8up60 onwards `-XX:+PreserveFramePointer` enable Frame Pointer.

* Can use `profile` in combination with **FlameGraph** tool to get visual insights.

#### Bash Shell

* Targeting Bash & shared libraries `ldd $(which bash)`.

#### Other Languages

* JS (node.js/v8) run things similar to Java. V8 has built-in USDT probes.

* C++ traced almost similar to C. C++ objects need to be declared in BPF program.

* For Go, botg gc & gccgo support Frame Pointer by default. Entry to Fn can be traced like `bpftrace -e 'uprobe:/home/bgregg/hello:fmt* { @[probe] = count(); }'`. Salp lib provides dynamic USDT.

---
