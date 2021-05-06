
> Begins Part.II **Concurrent Programming**

## Chapter.15 Working with Multiple Processes

> code samples for this reside in [chapter-15](./chapter-15) dir

* thanks to BEAM, creating multiple processes (these are not native OS process, but Erlang Process with little overhead) doesn't have a performance penalty

* uses Actor model for concurrency

> Actor is an independent stateless process that shares nothing with other processes.
>
> Can `spawn` new processes, `send` messages to them and `receive` messages back. That's it.


### A Simple Process

* `spawn` kicks off a new process; comes in many forms

> * popular ones let run anonymous OR named function, passing an arg list as for example in `Spawn.greet/0` at [spawn-basic.ex](./chapter-15/spawn-basic.ex)
>
> * `spawn` returns a process identifier (PID) uniquely identifying process created in application

* `spawn` creates a new process to run code; don't know when it will run just that it's eligible to... thus `message` is used to sync-up

#### Sending messages between processes

> modules `SpawnMsg` & its `Client` at [spawn1.exs](./chapter-15/spawn1.exs)

* uses `receive` to wait for message, then matches in block; in `SpawnMsg.greet/0` it's 2-element tuple of original sender's PID and message

* the match uses original senders PID in `sender` to `send` a formatted string back for operation; with `:ok` as first element

* this then matchs in `receive` block being waited upon after `Client` has `send` an earlier message to PID for `SpawnMsg`

#### Handling multiple messages

> modules `SpawnMultiMsg` & its `MultiMsgClient` at [spawn2.exs](./chapter-15/spawn2.exs)

* sending another messager to same PID of `SpawnMsg.greet/0` once used would get stuck waiting; that's beacuse `SpawnMsg.greet/0` exits on receiving once

* using `MultiMsgClient.greet_timeout/0` shows usage of `after` pseudopattern that lets exit on a millisecond timeout

* natural way to handle multiple messages by same PID would be using recursion as in `SpawnMultiMsg`

#### Recursion, Looping and the Stack

* tail-call optimization in `SpawnMultiMsg.greet/0`  avoids adding a new frame to stack atop previous calls

* adding an accumulator parameter to `defp do_<foo>` call for `def <foo>` style of using; one can create a tail-recursive call


### Process Overhead

> modules `Chain` at [chain.exs](./chapter-15/chain.exs)

* code at `Chain` creates `n` processes and checks time taken while doing it

> * it has `code_to_run` inner function block which spawn `counter` that is run in separate process
>
> * the `Enum.reduce` block does the magic; passes self to first `counter` and first process' PID to 2nd and so on; this way `last` gets reduced last process' PID
>
> * now it sends `0` to last process; which adds one to passed value and send it to previous PID and so on; this makes first process send total counter value to `create_procs`

* will fail for default limit for process before hard-limit is reached unless `elixir --erl "+P 1000000" -r chain.exs -e "Chain.run(500_000)"`

> even now if process fails for limit, that's hard limit for your system


### When Processes Die

> in [linked-proc.exs](./chapter-15/linked-proc.exs); for `Link1` top-level got no notification when spawned proc exits

#### Linking 2 Processes

* in `Link2` we spawn using `spawn_link` which links new process to caller, on abnormal exit `exit(:boom)` kills entire app

* Elixir uses OTP f/w to create process trees with supervision

* `OTP Supervisors` are covered later; for now can trap the exit to convert exit signals from linked signal into a message as in `Link3`

#### Monitoring a Process

* `monitoring` let's a proc spawn another proc to be notified of its exit/fail as `:DOWN` message but without vice-versa

* use `spawn_monitor`; can use `Process.monitor` (like in link to external proc) but with risk of race

* `spawn_monitor` (& `spawn_link`) ae atomic versions, will always catch a failure

* `spawn_monitor` returns a `#Reference` record in addition to PID as identity of monitor created


### Parallel Map - The "Hello, World" of Erlang

> pmap applies a function to each element in a separate process

* first enumerate through all elements to `spawn_link` the applying of map-function; then enumerate list item times to receive the results being collected into result

* this will improve once we go through agent and tasks


### A Fibonacci Server

* the same old fibonacci series sstarting with `0,1` where every other number is sum of previous 2

* here we'll write a parallel calculation code using a trivial server process that calculates & a scheduler that assigns work to process if free

```
 ,+--------+,   {:free, :pid}          ,+-----+,
 |         ^|-------------------------->| Sched |
 | Fib    //|                           |\      |
 | Server / | {:fib, n, sched_pid}      | \     |
 |       |  |<--------------------------|  \    |
 |       |  |                           |   \ when no work
 |        \ | {:answer, n, fib(n), pid) |    \  |
 |         \|-------------------------->|     | |
 |          |                           |     | |
 |          | {:shutdown}               |     | |
 |          |<--------------------------| <---' |
 '+--------+'                           '+-----+'

```

* `FibSvr`

#### The Task Scheduler

* `Sched.run/4` ignorant of actual task; spawns number of processes and records their PIDs, then passes it to `Sched.schedule/3`.

* `Sched.schedule/3` is a receive loop; if get `:free` then passes any work in queue; if queue is empty it sends `:shutdown`; if it's last process it sorts the list and we are done

* `FibSvr` sends answer to this receive loop for each calculated Fibonacci sequence

> `Sched.drive` drives the test run 10 times to calculate 20 times 5000 sequences in Fibonacci


### Agents - A Teaser

* sometimes there are lots of same internal calls made during function calls causing duplication; what if intermediate values could be cached

* Elixir modules can't hold state; but Processes can. A library module `Agent` makes it easy to wrap a process containing state in a nice module interface

> will be covered in detail later


### Thinking in Processes

* so far we are running our processes in same VM, to be able to scale get Nodes

---
