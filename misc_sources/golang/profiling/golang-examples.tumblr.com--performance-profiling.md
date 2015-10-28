## Performance Profiling

[source](http://golang-examples.tumblr.com/post/80979415324/performance-profiling)

#### Use runtime/pprof package

```
func main() {
    if (useProfile) {
        f, err := os.Create("proffilename.prof")
        if err != nil {
            panic(err)
        }
        pprof.StartCPUProfile(f)
        defer pprof.StopCPUProfile()
    }
...

```

Run ‘go tool pprof’ to analyze prof file.
```
$ go tool pprof <APPNAME> <PROFILENAME>
Welcome to pprof!  For help, type 'help'.
(pprof) top10
```
---
---
