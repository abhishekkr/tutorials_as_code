
## Seven Ways To Profile Go Applications

> by Dave Cheney
> at Golang UK Conference 2016


#### 1.) time

first method of profiling any program is `time`

```
time go fmt std
```

* shell built-in, GNU comes with time(1 `/usr/bin/time`) command i.e. more powerful than shell builtin

> showing info like pages of memory, swapping in effect

```
/usr/bin/time -v go fmt std
```


#### 2.) go build --toolexec

* `-x` prints all build actions happening in background

```
go build -x main.go
```

* `--toolexec <cmd-args>` is a build task flag used to invoke toolchain programs like vet, asm and toolstash.

```
if [[ $(uname -s) == "Linux" ]]; then
  go build --toolexec="/usr/bin/time -f '%U %S %C'" cmd/compile/internal/gc
elif [[ $(uname -s) == "Darwin" ]]; then
  go build --toolexec="/usr/bin/time" cmd/compile/internal/gc
else
  echo "Who art thou!"
fi
```


#### 3.) GODEBUG flag

Go runtime collects various stats during program lifecycle. One can enable their display by enabling GODEBUG.

* Simple way to obtain general idea of how hard GC works is to enable output of GC logging

```
env GODEBUG="gctrace=1" godoc -http=:8080
```

---

### Profilers

A SIGPROF signal is send to program, program is interrupted, control is transferred back to profiler and stats are gathered.

Profiling Do's and Dont's

* machine must be idle, no profiling on shared hardware

* watch for power saving and thermal scaling

* avoid VMs and shared cloud hosting, too noisy

* kernel bug on OSX versions, no upgrades between profilings

* one profiling at a time, if measuring program not machine

---


#### 4.) pprof

Descends from Google Perf Test Suite, built into Go runtime.

Comes in two parts

* `runtime/pprof` package built into every Go program

* `go tool pprof` for investigating profiles

It helps with `CPU Profiling`, every 10ms record stacktrace of current goroutines. The more times a function appears in profile, the more time code path is taking as a percentage of total runtime.

With `Memory Profiling` records stacktrace whena heap allocation is made. It's sampling 1 in every 1000 allocations, rate can be changed. It tracks allocation not use.

`Block profiling` is unique to Go. Similar to CPU profile, but records goroutine waiting on shared resource not running. Useful to determine concurrency bottleneck.

##### Microbenchmarks

Easiest way to profile a function is `testing` package. Testing package has built-in support for generating CPU, memory and block profiles.

```
-cpuprofile=$FILE -memprofile=$FILE -memprofilerate=N -blockprofile=$FILE
```

```
go test -run=XXX -bench=IndexByte -cpuprofile/tmp/c.p bytes
go tool pprof byte.test /tmp/c.p
```

Use `-run=XXX` to disable tests, only to profile benchmarks

##### Profiling whole programs

`github.com/pkg/profile` to easily profile an application

```
import "github.com/pkg/profile"

func main(){
  defer profile.Start().Stop()
}
```

##### /debug/pprof

if program runs a webserver, can enable debugging over http

```
import _ "net/http/pprof"

func main(){
  log.Println(http.ListenAndServe("localhost:8080", nil))
}
```

Then use pprof tool to look at

```
# a 30-sec CPU profile
go tool pprof http://localhost:8080/debug/pprof/profile

# heap profile
go tool pprof http://localhost:8080/debug/pprof/heap

# goroutine blocking profile
go tool pprof http://localhost:8080/debug/pprof/block

```

##### Using pprof

Should always be invoked with 2 arguments

```
go tool pprof /path/to/binary-generating-profile /path/to/profile-generated
```

Just printing is hard to understand, use `web` or `svg` to visualize.

---

### Framepointers

Go1.7 enables frame pointers by default. It's a register that always points at current stack frame.

It enables tools like `gdb` and `perf` to understand Go call stack.

---

#### 5.) perf

As a linux user `perf` helps profiling applications.

```
go build -toolexec="perf stat" cmd/compile/internal/gc
```

Can record `perf` insight

```
go build -toolexec="perf record -g -o /tmp/p" cmd/compile/internal/gc

perf report -i /tmp/p
```


#### 6.) Flame Graph

* x-axis showing stack of population, sorted alphabetically

* y-axis showing stack depth

* each reactangle is a stack frame, wider represents frequency of presence in stacks

* top-edge shows what's on CPU, beneath is ancestry

Nice ideas from Brendan Gregg.

Uber released [torch](github.com/uber/go-torch) to process pprof endpoints or existing profiles.

```
go build -gcflags=-cpuprofile=/tmp/c.p .

go-torch $(go tool compile -n) /tmp/c.p
```


#### 7.) Go Tool Trace

In Go 1.5, Dmitry Vyukov added a new kind of runtime profiling. Execution trace profiling.

Gives insight into dynamic execution of a program. Captures with nano-second precision:

* goroutine creation/start/end

* goroutine blocking/unblocking

* network blocking

* system calls

* GC events

*It is highly undocumented.*


---
---
