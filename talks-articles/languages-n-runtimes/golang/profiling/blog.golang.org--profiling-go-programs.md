## Profiling Go Programs

[source](http://blog.golang.org/profiling-go-programs)

* time the execution

```
time -f '%Uu %Ss %er %MkB %C' "$@"
```

---

#### enable profiling

Using [Go testing package](http://golang.org/pkg/testing/) benchmarking support, we could use gotest's standard '-cpuprofile' and '-memprofile' flags.
In a standalong program, need to import 'runtime/pprof' and add few lines:
```
var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")

func main(){
  flag.Parse()
  if *cpuprofile ! = "" {
    f, err := os.Create(*cpuprofile)
    if err != nil {
      log.Fatal(err)
    }
    pprof.StartCPUProfile(f)
    defer pprof.StopCPUProfile()
  }
}
```

---

to profile
```
$ go build myprogram.go
$ ./myprogram
$ go tool pprof myprogram $cpuprofile
(pprof) top10
```

* topN is a common informative command showing top N sample in the profile

* the first 2 col show sample count in which function was running, 3rd col show running total during listing, 4th and 5th col show number of samples in which function appeared (running or waiting)

* to sort by 4th and 5th append '-cum' flag

* when cpu profiling is enabled, program stops 100times/second to sample program counters on current goroutine's stack

---

* stacktrace contain call relationship, this graph can be viewed using 'web' command which writes a graph for profile ddata in SVG format and opens in web-browser

* 'gv' command is Ghostview's PostScript alternative of 'web' command. Both alternatives need 'graphviz' installed.

* can tell 'web' to use only samples including specific function as 'web runtime.abc'

---

Say 'main.DEF' is using loads of memory, can run 'list DEF' at pprof to find memory allications.

---
---
