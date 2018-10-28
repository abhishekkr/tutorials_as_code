
## Chapter 1.3 Concurrent Programming

### 1.3.1 Processes

* `process` term is usually used when threads don't share data, term `thread` when they share data
> threads of execution in erlang share no data, thus called `processes`

* erlang BIF `spawn` creates a new process `spawn(Module, Exported_Function, List_Of_Arguments).`

* module [chapter1-3-01.erl](./chapter1-3-01.erl) has function `start()` spawning two processes `say_something()`, both need to be exported
> * notice all `hello` prints aren't continuous
>
> * `<0.66.0>` or whatever you get in next runs, are return value from last command in function `start()` that is the process identifiers (`pid`)
>
> * when used `spawn()`, returned is `pid` of spawned process

```
1> 'chapter1-3-01':start().
hello
hola
<0.66.0>
hello
```

* also on unrelated note, `-compile([{nowarn_unused_function, [{ fnName,fnArity }]}]).` allows supress unexported/unused function warning


### 1.3.2 Message Passing

* to send message to process with pid `Pid_X`, use `Pid_X ! Message`
> `!` operator returns the message sent so can be used to send multiple message at once as

```
Pid_X ! Message, Pid_Y ! Message, Pid_Z ! Message

Pid_X ! (Pid_Y ! (Pid_Z ! Message))

Pid_X ! Pid_Y ! Pid_Z ! Message
```

* to empty message queue, `flush/0` BIF could be used

```
1> PX = self(), PY = self().
<0.76.0>

2> PX ! PY ! {some, msg}.
{some,msg}

3> flush().
Shell got {some,msg}
Shell got {some,msg}
ok
```

* module [chapter1-3-02.erl](./chapter1-3-02.erl) creates two processes which send messages to each other
> * simple function `echo()` works on message passing from `start()`
>
> * there is no `;` right before `end`
>
> * Here `pong()` is called, `ping()` runs for a counter with Pong's PID and sends `ping, self-pid`. When `pong()` receives `ping, ping_pid` it passes back `pong` message as well as loops back to receive call. This goes on till `ping()` counter reaches zero then `ping()` doesn't recrusively calls itself and send `finished` to `pong()` to end.


```
1> 'chapter1-3-02':start().
; welcome
Pong received ping
bye
; "say what"
Ping received pong
; "say something"
Pong received ping
; bye
Ping received pong
ping finished
Pong finished
```

* messages could be any valid erlang terms

* each process has its own input message `queue`
> erlang has clever queue implementation to minimize times each message is tested against patterns in receive, read more on it


### 1.3.3 Registered Process Names

* in previous section module, `pong()` was first created so its identity (pid) could be provided to `ping()`
> * this is tricky when both processes need to know each other's identity
>
> * even trickier if they have been started independently of each other

* erlang lets you name processes, these names can be used as identities instead of Pid to pass messages
> done by using BIF `register` as `register(some_atom, Pid)`

* module [chapter1-3-03.erl](./chapter1-3-03.erl) re-implements ping-pong using register for `pong()`
> * there is also a sample bob-eve message passing
>
> * `{"bob", "bye"}` is just return from `start()`

```
1> 'chapter1-3-03':start().
@"eve": what is "hola"
@"bob": what is "ahoy"
ping got pong
bob left the channel
{"bob","bye"}
eve left the channel
ping got pong
ping finished
no more pong
```


### 1.3.4 Distributed Programming

* To do message passing across machines
> * distributed Erlang implementation provides a basic authentication mechanism to prevent unintentional access using `magic cookie`
>
> * easiest way to achieve auth is by having a file called `.erlang.cookie` in home dir
>
> * on all machines they have same atom and `0400` file permissions

* Need to give every erlang system a name, like `erl -sname my_name`.
> * Using `-sname` assumes all nodes are in the same IP domain, to use in different domains use `-name`
>
> * Each erlang system running is called an `Erlang node`
>
> * Can run on same machine with different node names, to imitate different Erlang systems

* module [chapter1-3-04.erl](./chapter1-3-04.erl) for multi-node `ping-pong` example rewrite
> * `start_ping()` would need pong's `<erl-sname>@<machine-name>` as a parameter
>
> * the different notation for message passing is a tuple `{registered_name, node_name}` instead of just `registered_name`
>
> * `start()` if ran on `xpong` node would remotely spawn `ping(N, Pong_Node)` on `xping` or whatever node passed; but in this case all i/o will occur on `xpong` where the process spawned from

```
$ erl -sname xping
(xping@machineABC)1> c('chapter1-3-04').
{ok,'chapter1-3-04'}


$ erl -sname xpong
(xpong@machineDEF)1> c('chapter1-3-04').
{ok,'chapter1-3-04'}


(xpong@machineDEF)4> 'chapter1-3-04':start_pong().
true
pong got ping
pong got ping
pong got ping
pong finished


(xping@machineABC)5> 'chapter1-3-04':start_ping('xpong@machineDEF').
<0.85.0>
ping got pong
ping got pong
ping got pong
ping finished
```


### 1.3.5 A Larger Example

* a simple messenger, allowing users to chat from different nodes

* OTP facilities haven't been used here, which would make the solution easier, robust and reliable

* first program contains issues handling disappearing nodes, corrected in later version

* messenger is set by allowing `clients` to connect to a central server and login, so they don't need to know of each other

* module [chapter1-3-05.erl](./chapter1-3-05.erl) has server/client code

> * `server_node()` function employs server identity, need to be updated if needed be changed
>
> * compiled code `chapter1-3-05.beam` can be copied to each machine, where erlang needed started
>
> * server `messenger` side logs

```
 % erl -sname messenger
Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3.5.5  (abort with ^G)
(messenger@Cr4Sh-0v3rrid3)1> c('chapter1-3-05').
{ok,'chapter1-3-05'}

(messenger@Cr4Sh-0v3rrid3)12> 'chapter1-3-05':start_server().
true
list is now: [{<10229.71.0>,eve},{<10227.71.0>,bob}]
list is now: [{<10229.71.0>,eve},{<10227.71.0>,bob}]
```

> * client `c1` and `c2`  side logs

```
± % erl -sname c1
(c1@Cr4Sh-0v3rrid3)1> c('chapter1-3-05').
{ok,'chapter1-3-05'}

(c1@Cr4Sh-0v3rrid3)2> 'chapter1-3-05':logon(bob).
true
logged_on

(c1@Cr4Sh-0v3rrid3)3> 'chapter1-3-05':message(eve, "hola").
ok
sent
@eve: "ahoy"

(c1@Cr4Sh-0v3rrid3)2> 'chapter1-3-05':logoff().
```

```
± % erl -sname c2
(c2@Cr4Sh-0v3rrid3)1> c('chapter1-3-05').
{ok,'chapter1-3-05'}

(c2@Cr4Sh-0v3rrid3)2> 'chapter1-3-05':logon(eve).
true
logged_on
@bob: "hola"

(c2@Cr4Sh-0v3rrid3)3> 'chapter1-3-05':message(bob, "ahoy").
ok
sent

(c1@Cr4Sh-0v3rrid3)2> 'chapter1-3-05':logoff().
```

* in the module here we'll notice use of multiple `lists` functions

* a process terminates when nothing to do, can be terminated using `exit/1`

* `whereis/1` is used testing if a client process exists

---
---
