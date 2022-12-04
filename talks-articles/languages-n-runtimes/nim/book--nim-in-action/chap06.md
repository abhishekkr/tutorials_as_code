
## Chapter.6 Parallelism

> [code samples](./code-samples/chap6.nim)

### Concurrency vs Parallelism

* Async-Await is concurrent, Spawn is Parallel. Nim Threads for contexts that share a lot of memory and can be both parallel & concurrent.


### Using threads in Nim

* [threads](http://nim-lang.org/docs/threads.html) module to create threads manually; implicitly imported with `system` module

* [threadpool](http://nim-lang.org/docs/threadpool.html) module exposes `spawn` which adds a specified proc to thread pool's task queue (doesn't mean proc will run in a separate thread immediately, creation is managed by thread pool)

* each Nim Thread has isolated Heap mem & GC, restricting sharing and preventing race condition alongwith improving efficiency (no locking of all threads for GC)

```
var daa = "This is Data"                 #--> mutable global var

## below would cause errors when compiled as Global data would be accessed directly
## and wouldn't be available, it need to be passed

## proc showDaa() {.thread.} =           #--> proc using pragma
##   echo(data)
##
## var thread: Thread[void]              #--> generic parame here void
## createThread[void](thread, showDaa)   #--> creating thread passing proc to run

proc showDaa(d: string) {.thread.} =     #--> new proc for thread using pragma
  echo(d)

var thread: Thread[string]                  #--> new thread var with generic parame, here string
createThread[string](thread, showDaa, data) #--> passing the data to be used in proc
joinThread(thread)                          #--> waiting for thread to finish

## for multiple threads
var threads: Thread[3, string]
createThread[string](thread, showDaa, "1")
createThread[string](thread, showDaa, "2")
createThread[string](thread, showDaa, "3")
joinThread(thread)
```

> would be compiled as `nim c --threads:on file.nim`

* a proc is GC safe as long it doesn't access another thread's GC viable mem

* `createThread` can only pass 1 var to thread, to pass multiple it would need to pass a custom Type; different order might be run given OS, system arch, etc.

* `threads` module's thread are resource intensive; for large thread count `threadpool` is superior

* `thread` creation is expensive and just creating lots of them might end up slowing perf; just few as per core count might leave tasks queued up while active threads are idle waiting on I/O

* `threadpool` implements an abstraction that manages distribution of tasks over a number of threads, ensuring all threads are busy

* `spawn` accepts an expression like proc call and returns value of type `FlowVar[T]`

* if the thread crashed, main app would crash even if invoked in try-except; (planned to change in future for crash when read FlowVar)

* `{.raises [].}` pragma gets used if want to add a check which exceptions are allowed in a thread and handle them preemptively, to have a safer path

* for long running tasks `createThread` from `threads` module should be used


### Parsing data

* 3 methods: regexps, `split` proc, `parseutils` module

* Regexps supported via `re` impure module depending on C lib `PCRE` and should be installed alongside your app

* performing multiple regexp is expensive; can use `replace` proc to replace matched items

* `split` proc is available via `strutils` module

* `parseutils` module provide `parseUntil` proc making parsing easier; there are other convenient procs as well primarily a wrapper over `while` loop

* better to read large data in chunks; `lines` iterator from `system` module can be ised over file

* `s.setLen(0)` is more performant than `s = ""` as former reuses memory; so to be used when resetting var primarily if the reset has to happen numerous times (maybe even thousands or millions based on record count)

> for compute intensive tests, always compile with `-d:release` to keep runtime compact


### Parallelizing Parser

* like parsing each line (or whatever block is contextual) to be parsed separately

* instead of readinline lines/blocks, read a large fragment and parse these blobs

* slices shouldn't have incomplete lines/fragments; save incomplete line/block and prefix it to next read fragment

> [code sample](./code-samples/chap6_4.nim)


### Dealing with Race Conditions

* cross thread memory access ain't allowed for proc marked with `{.thread.}`

* shared memory is allowed and increases risk of race

* `lock` to limit resource access; when var locked with `guard` then compiler will check lock before allowing access

* `channel` (an implementation of FIFO `queue`); `channels` module is part of `system`

* `channel` is a shared variable allowing every thread to send/receive messages, defined as `var c: Channel[string] ; open(chan)`

* `recv` at `channel` is blocking, can use `tryRecv` for non-blocking

> [code sample](./code-samples/chap6_5.nim)


---
