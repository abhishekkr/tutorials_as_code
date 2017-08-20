

## From Parallel to Concurrent
> at Lang.NEXT 2014
> by Rob Pike, Google


### Part.I Sawzall (data at scale)

to fix log analysis problem

> designed a system `Sawmill`, RDBMS are hopeless at this scale
> needed realtime data engine, continuous update, a parallel-awk like requirement
> very early use of Dean and Ghemwat's MapReduce

> simple procedural language, strongly typed
> uninteresting programming model
> interesting query model with Tables for parallel system

#### Tables

generalized associative arrays

```
queriesByCountry: table sum[country string] of int;
emit queriesByCountry["china"] <- 1;
```

indexing dynamically evaluated; could be multi-dimensional

> many table types; sum, quantile, top, histogram, etc.


#### Execution Model

A Sawzall program defines computation of single record. Apply a single query to all records of interest. Program runs on thousands of machines in parallel (Map). Data collated on fewer machines, but also in parallel (Reduce).
Data dumped to a file, injected into DB, etc.


#### The Implementation

Sawmill collected data and ran batch Sawzall jobs at huge scale.

Language implementation:
* interpreter 4x faster than python
* later on-the-fly compiler
* custom protocol buffer parser for each instance
* arena GC
I/O throughput dominated performance.


##### Example: Geographical Distribution of Google Queries

```
proto "querylog.proto"

staic RESOLUTION: int = 60; # in minutes

log_record: QueryLogProto = input;

datum: table sum[t: time][lat: int][lon: int] of int;

loc: location = locationinfo(log_record.ip);

if (def(loc)) {
  t: time = log_record.time_usec;
  m: int = minuteof(t);
  m = m - m%RESOLUTION;
  t = trunctohour(t) + time(m*int(MINUTE));
  emit datum[t][int(loc.lat)][int(loc.lon)] <- 1;
}
```

* a missed opportunity

way to handle bad data

```
y = f(x);
if def(y) { emit tab[y] <- 1 }
```

#### What Sawzall got Wrong

no module, library or namespaces

aimed to low for small snippets

---

### Part.II Go Code at Scale

started a procedural language featuring with CSP
* GoRoutines
* Channels
* `select` statement


then everything else
* C-like structures
* pascal like declarations
* statically typed
* closures
* interfaces
* rigorous dependency management


procedural (a compromise), concurrency is a nice fit
but now worries of data races, so got tooling to check up on it

threads are hidden, stack-management hidden, locking hidden
so high-level concurrent programming is easier

runtime to manage threads transparently

GC frees mem, only GC does


a simple program with so much concurrency behind the scenes

```
package main

import (
  "net/http"
)

func main(){
  http.ListenAndServer(":8080", http.FileServer(http.Dir("/opt/content")))
}
```



---
---
