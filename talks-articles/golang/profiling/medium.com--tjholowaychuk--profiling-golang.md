## Profiling Golang

[source](https://medium.com/@tjholowaychuk/profiling-golang-851db2d9ae24)

The 'runtime/pprof' package offers lower level control over creating profiles.
The 'net/http/pprof' registering HTTP end-points for profiling live applications.

#### Use "github.com/davecheney/profile" to simplify

```
func main() {
  defer profile.Start(profile.CPUProfile).Stop()
...

```

To profiles
```
$ go build
$ go tool pprof -text ./myprogram
...
```

Can generate a PDF call-graph and things
```
$ go tool pprof -pdf ./myprogram
```

---
---
