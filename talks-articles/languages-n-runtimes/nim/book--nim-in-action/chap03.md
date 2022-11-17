
## Chapter.3 Writing a Chat Application

> Plan: a CLI asking input, talking to all other instances over Socket in common json contract; using modules
>
> [sample code for chat-app](./code-samples/chap3chat/src/)

### The Architecture of a Chat App

* P2P or Client-Server could be used

* Client-Server would be used here for simplicity over TCP for reliability

* `mkdir -p code-samples/chap3chat/{bin,images,src,tests}` as typical code structure


### Retrieving Input in Client Component

* `os` module avails `paramCount()` and `paramStr(idx)` for arg inferences; zero index would always have binary name as for shell

> * if binary name is needed, use `getAppFilename` to avoid os-specific data
> * `parseopt` module could help utilizing flags like `--help` more intuitively

* minimum data required in comms would be address to server, user id and message

* `stdout.write` to display a marker for input & `stdin.readLine` to accept input

* use `let msg = spawn stdin.readLine()` to avoid blocking I/O; msg isn't immediately available so read with `^msg`

```
while true:
  let message = spawn stdin.readLine()  # message gets FlowVar[string]
  doOtherWork()
  stdout.write(">> " & ^message & "\c\l")
```

> * using `spawn` requires compilation with `--threads:on` and `import threadpool`
> * can have a `client.nims` with compiler flag
> * otherwise `Ctrl + C` would show traceback, blocked at `readLine`.. thus would also not display received message
> * `spawn` returns value of type `FlowVar[T]` holding value of procedure spawned retrieved by `^` operator
> * `FlowVar[T]` provides `isReady` call to check if value is available

* `FlowVar[T]` is a `Generic` (detailed in Chap.9) type that can store any type


### Implementing the Protocol

* every `*.nim` file is a module; everything defined is private by default which can be made public using `*` operator at the end of procedure/variable/method/field names

> * will create a `protocol.nim` as message parser under `src`, with public type `Message`
> * `import json` and `parseJson(msgInstance)` would do the trick
> * `parseJson` returns a `JsonNode` variant type, its `str` field gives string value and `kind` field determines kind of json node; `JsonNodeKind` is an enum

> a variant type's fields change depending on value of one or more fields

```
## variant example
type
  Model = object
    case human: bool
    of true:
      name: string
    else:
      discard

let alice = Model(human: true, name: "Alice")
let shoes = Model(human: false)
```

> * while writing tests, use `doAssert` that's similar to `assert` but not optimized when compiled in release mode
> * Nim compiler compiles by default in debug mode that's a bit slow

```
## works like Python's __name__ == "__main__"
when isMainModule:
  block:
    ...
```

* can use `%` operator creating a new JsonNode object and convert to string using `$` operator for `createMsg` public proc

> * `{:}` is table constructor, sugar for array constructor; like `{"k": "v"}` is same as `[("k", "v")]`
> * adding `\c` carriage-return & `\l` line-feed


### Transferring Data using Sockets

* `server` module would listen for new connections and new messages from connected clients to be broadcasted

> * `Server` & `Client` type are reference objects so procedures can modify them
> * constructor naming convention is `initMyObj`, `initMyObjRef` and `initMyTuple` for object, ref object & tuple

* Sockets support `send` (write), `recv` (read), `connect, bindAddr` (open) & close like file-descriptors.

> * a TCP socket is neither server or client until `connect` or `bindAddr` is used at it
> * by default socket binds to localhost, port is always needed; need `listen` proc to start accepting calls with `accept` proc
> * async sockets (from `asyncnet`) don't block sockets on `accept` calls

* Nim supports many abstractions for async I/O, primarily making usage similar to sync I/O.

> * `accept` proc accepts server socket, it's sync implementation blocks the thread until a new client socket connects
> * its async implementation returns a `Future[AsyncSocket]` object

* `Future` is a special type (can also be called promise, delay, deferred). Every async op in Nim returns `Future[T]` object defined in `asyncdispatch` module.. can also be tried without async op. Easily build your own async behavior objects.

```
import asyncdispatch

var futureV = newFuture[int]()
doAssert (not futureV.finished)
futureV.callback =
  proc (f: Future[int]) =
    echo("future is no longer empty, ", f.read)
futureV.complete(42)
doAssert futureV.finished
```

> * `future.fail(newException(ValueError, "The futureV failed."))` would raise the issue at read
> * could use `failed` proc that returns a bool denoting if future promise is kept
> * unless explicitly read, future disappears when deallocated; so don't `discard` future but `asyncCheck` to ensure exceptions are reraised in main thread

* `doWork` proc would work while `accept` hasn't received a socket in async mode

```
## sync
 [server]--->[accept]--(Thread Blocked)-->{Client-Socket}->[doWork()]-->[doWork(socket)]
 [socket]      '|____________________________________________________________|

## async
                                       ,'-------------------,
 [server]--->[accept]--(Future)-->{Future.empty?}-,-true->[doWork()]
 [socket]     '|                                 false->[Future.read]-->{Client-Socket}->[doWork(socket)]
               |____________________________________________________________________________|
```

* above async flow has issue of `busy waiting` where it keeps checking whether Future is empty; using `callback` here would prevent that

```
# example with callback
import asyncdispatch, asyncfile

var file = openAsync("/path/to/file")
lat dataFut = file.readAll()
dataFut.callback =
  proc (fut: Future[string]) = echo(fut.read())
asyncdispatch.runForever()
```

> * NodeJS's runtime is form of event-loop using native system APIs to check for events; NIM's event loop is defined by `asyncdispatch` module and ran explicitly (thus use of `runForever`)
> * can call `poll` proc for greater control, it waits for specified amount of time so isn't exactly instantaneous and thus sort-of blocking

* for complex logic flow `callback` are not always flexible and might require hell-ish nesting; thus `async await`

```
# example with async await
import asyncdispatch, asyncfile

proc readWriteFile(fpath: string) {.async.} =
  var file = openAsync(fpath, fmReadWrite)
  lat dataFut = await file.readAll()
  echo(dataFut)
  await file.write("Hola!\n")
  file.close()

waitFor readWriteFile("/path/to/file")
```

> * `await` waits for future to complete, while proc waits the app continues to run
> * every proc marked `{.async.}` returns `Future[T]` which can be waited for with `waitFor` before app exits
> * `ayncCheck` sets specified future's callback property to a procedure that will handle exceptions properly

* let's use `await` & async sockets for chat server


---
