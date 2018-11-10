
## Chapter.1 The Erlang/OTP platform

> OTP is always included in an Erlang distribution and an integral part.
>
> OTP was originally acronym for Open Telephony Platform, but nothing specific to telecom applications. It's more of a Concurrent Systems Platform.

Some main advantages of OTP are productivity, stability, supervision (automatic and via GUI), upgradability and reliable codebase.

This chapter shows core concepts of Erlang/OTP platform that everything else in OTP builds on:
* Concurrent programming
* Fault tolerance
* Distributed programming
* The Erlang VM and runtime system
* Erlang's core functional language


### 1.1 Concurrent programming with processes

Erlang was designed for concurrency, uses process for clean isolation between tasks with fault tolerance.

#### 1.1.1 Understanding concurrency

> Things that don't have anything forcing them to happen in specific order, are concurrent.

In case of extra CPUs, Erlang processes concurrent tasks in parallel. Processes have their own working memory and separate mailbox. So if a process crashes its resources can be recycled without affecting others from same parent.

#### 1.1.2 Erlang's Process Model

Process share no internal data, so communicate by making a read-only personal copy.

`Transparent distribution` allows to look at network as a collection of resources, method of communication is exactly same no matter where they're located.

#### 1.1.3 Four process communication paradigms

##### Shared Memory with Locks

Shared memory could reasonably be called the GOTO of our time, a mainstream technique for process communication. To restrict only one process access a cell for atomic operation, done with `lock`. Implementing locks require memory system support, esp. in form of hardware special instructions. Higher level constructs like semaphores, monitors and mutexes.

This avoids worst problems, locks still have a number of drawbacks

* locks require overhead even when the chances of collisions are low

* they are point of contention in memory system

* can stay in locked state by failed processes

* hard tp debug issues, problems grow with number of processes sharing

##### Software transactional memory (STM)

A nontraditional method, found in GHC and Clojure. It treats memory like a DB using transactions to decide what gets written and when. It plays an optimistic hand, if two processes try take a lock then one succeeds and other has to retry post checking new contents. No locks required.

Main drawback is retrying failed transactions, which could fail repeatedly. Better than locks as allow better concurrency.

##### Futures, Promises and similar

Have several variants. Can be found in E, MultiLisp; as lib in Java. Similar to I-vars and M-vars in Id and GHC, concurrent logic variables in Concurrent Prolog, and dataflow variables in Oz.

Here `future` is a result of an outsourced computation. Future can be passed around like an object, readers have to wait till it's ready.

Although failure case need be handled when process try access value of promise but it's still missing and connection/delegated-task is dead.

##### Message Passing

Receiving process gets a separate copy of data. The only way to communicate info back is send another message in reverse direction.

It comes in two flavors, synchronous and asynchronous. In synchronous form sender waits till message arrives at receiving end, receiver sends an acknowledgement to sender. Asynchronous model is `send-and-pray`. Though acknowledgement is overrated as receiver might die right after sending acknowledgement.

Erlang's message passing primitives are asynchronous, which can be turned synchronous by making sender wait for receiver's reply. Better to manage failures and retry by a separate accountability queue.

Copying data can be expensive for large structures and lead to high memory usage if sender need to sustain a copy as well.


#### 1.1.4 Programming with processes in Erlang

Concurrency is cheaper in Erlang, a new process costs as much as creating a new object in average object-oriented languages. With process end it takes away all internal state, not to be dealth with manually.

##### Creating a process: spawning

Erlang runtime is capable of easily spawning lacs of processes on single simple system. One process going berserk can't corrupt another process due to full isolation.

Erlang processes are not OS threads. A typical thread reserve some MB of address space for its stack, Erlang processes start with only a couple of hundred bytes of stack space each and can resize automatically.

Simple way to spawn a process is following, there are few variants as well

```
%% to spawn  'io:format("booyaah!").'

spawn(io, format, ["booyah!"]).
```

##### How processes talk

Erlang processes exchange info via message passing using `!` 'bang' operator. Syntax is `Destination ! Message`. Simple example following, running sender process spawns a receiver process and then waits for receiver to pass back ack message.

```
sender() ->
  Pid = spawn(fun receiver/0),
  Pid ! self(),
  receive
    ack -> ok
  end.

receiver() ->
  receive
    From -> From ! ack
  end.
```

Every process has a mailbox and arriving messages are kept there until checked for.

OTP framework takes message passing to another level, explored in Chapter3.

##### Process termination

On termination, process' memory, mailbox and other resources are recycled.

Crashes can make a process terminate before it's finished working, here other processes can be informed of the crash to manage cascading effect.

---

### 1.2 Erlang's fault tolerance infrastructure

Erlang has traditional exception handling like other languages, also has a unique system of process links for handling process failures in concurrent models effectively.

#### 1.2.1 How process links work

Different processes can link to each other on an abstract layer. When any process dies, all linked processes get `exit` signal generated by dying process causing their exit as well. This cascading behavior allows a group of concurrent tasks to act as one running entity.

This allows entire subsystem to be restarted from stable state without worrying about leftover processes.

> Erlang's philosophy is _Let it crash_, instead of thrashing around blindly. Drop everything, start over.

#### 1.2.2 Supervision and trapping of exit signals

One of main faul-tolerant ways achieved in OTP is by overriding default propagation of exit signals by setting process flag `trap_exit`.

Received singals are dropped in process' mailbox in form `{'EXIT', Pid, Reason}`, allowing the process to internally manage the action flow on such signal.

Generally signal-trapping is done in a special process termed `system process`, typically running code different than worker processes. Such process can be made responsible of insulating process linked to it from each other, reporting failures and restarting failed subsystems. Such system processes are called `supervisors`.

OTP framework provides both a methodology for structuring applications using supervision, and stable battle-hardened libraries to build them on. Supervisor starts a process in a prescribed manner and order. Supervisor can be told how to restart its processes with respect to each other in failure; retry counts and waiting period for restart can be set as well.

System shouldn't be structured single-level hierarchy but a supervision tree with multiple layers that allow subsystems, allowing restart at differnet layers.

#### 1.2.3 Layering processes of fault tolerance

Layering ties related subsystems together under common supervisor, provides different levels of working base states that you can revert to.

---

### Distributed programming

Erlang programs can be distributed over multiple computers as running on same if build using proper concurrent processes and message passing. This makes developing, debugging and evolving distributed solutions easier as single node solutions.

Spawned erlang processes have unique IDs (combination of erlang instance name and hostname) valid on node and network, mailbox listens on that unique ID locally and on network. So message passing works similarly on localhost as on network.

Getting these built-in functionalities in most other languages is a complex non-standard library task.

---

### The Erlang VM and runtime system

Core of standard Erlang implementation is ERTS (Erlang Run-Time System) application. ERTS is written in C, responsible for all low-level stuff.

Erlang VM emulator is an important part of ERTS, it executes pre-compiled bytecode of erlang programs. This VM is BEAM (Bogdan's Erlang Abstract Machine). It's possible to compile Erlang programs to native machine code, but BEAM is fast enough for no need of it.

#### The scheduler

Handles running of Erlang's processes to share resources. Waking up sleeping process when they receive a new message or on timeout.

ERTS generally runs as an OS process, under name `beam` or `werl`.

SMP (Symmetric MultiProcessing added in 2006 with release 11) allowed Erlang runtime system to use multiple schedulers internally, each using a separate OS thread.

It's possible to tie processes to schedulers depending on CPU topology of machine, to better utilize cache architecture of hardware.

Newbies might rely on effects of timing, which might work on dev machines but break on multicore production nodes with less deterministic timings. Testing is required.

#### I/O and scheduling

Prevents entire system from stopping just because some process need external communication.

Erlang scheduler does all I/O in event-based way, let's program handle each data as it enters in a non-blocking way. It removes need of OS-based locking or context switching. Optimal for high availability and low latency systems, though hard to reason with people about.

It integrates event-based I/O system with its process scheduler.

#### Process isolation and the GC

Keep recycling memory not needed anymore. Erlang currently uses straightforward generational copying grabage collector.

Programs don't tend to suffer with GC pauses when designed keeping Erlang's functional philosophy to core. Process got isolation, program doesn't pause when individual programs get GC and their memory is not that huge as well due to concern split. Also if process haven't been active since last GC, it can be skipped. For quickly finishing process, never may need GC.

---

### Functional programming: Erlang's face to the world

Concurrency is defining feature of Erlang, FP is an important aspect of it.

---
---
