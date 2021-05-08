
## Chapter.17 OTP: Servers

> first OTP server code resides at [sequence17](./sequence17)

> it isn't answer to all high-availability distributed app woes, but do solve a lot issues

* OTP (Open Telecom Platform) is a bundle that includes Erlang, db (Mnesia) and many a libs


### Some OTP Def

* OTP defines systems in hierarchies of `applications`; an application contain `>=1` process which follow small number of OTP conventions called `behaviors`

* there is a behavior for general-purpose server; one for event handlers; one for finite-state-machines

* each implementation of these behaviors run in its own process and may have associated additional processes

* a special behavior `supervisor` monitors processes health and implements strategies to restart if needed

> in this chapter, we'll implement `GenServer` behavior


### An OTP Server

* when we write an OTP server, we write a module containing one+ callback functions with standard names; appropriate callback for situtation will be invoked by OTP

* when someone sends request to server, OTP will call `handle_call` function passing in {request, caller, current-server-state} and responds tuple {action-to-take, return-value, update-state}

#### State and the Single Server

* servers can use recursion; handling one request each call with getting passed the current state

* returning a maybe updated state which gets passed on to next request handler

#### Our First OTP Server

> creating simple OTP server; get passed a number on start as current state; gets called with `:next_number` request returns current state and incrment it... then rolls around at 17

* create new project `mix new sequence17`

* create `sequence17/lib/sequence17/server.ex` for `Sequence17.Server` server module

> * `use GenServer` adds OTP GenServer behavior defining default callbacks for the module
>
> * `init/1` taking initial value and constructs the server state
>
> * `handle_call` receives request info, client PID & server state

* fire up server manually `iex -S mix`

```
% iex -S mix
Erlang/OTP 22 [erts-10.7.2.9] ..

Compiling 2 files (.ex)
Generated sequence17 app
Interactive Elixir (1.9.2) ..

iex(1)> {:ok, pid} = GenServer.start_link(Sequence17.Server, 100)
{:ok, #PID<0.165.0>}

iex(2)> GenServer.call(pid, :next_number)
100
iex(3)> GenServer.call(pid, :next_numbers)

14:09:41.312 [error] GenServer #PID<0.165.0> terminating
```

> * no error-handling or general request matcher, so Call fails on any request other than `:next_number`

* to pass multiple things in state, use tuple

* can have multiple handlers using different matcher for `handle_call`

> `Sequence17.StackServer` implements Stack for `Exercise: OTP-Servers-1 & 2` within [stackserver.ex](./sequence17/lib/sequence17/stackserver.ex)

* `GenServer.call/3` calls the Server and waits on reply; can make a `GenServer.cast/2` to cast an async request without waiting on reply; these are handled by `handle_cast` functions though so not interchangeable

* `handle_cast` reply with `{:noreply, new_state}` as none is waiting on a reply; example is `Sequence17.StackServer.handle_cast/2`

```
iex(1)> {:ok, gpid} = GenServer.start_link(Sequence17.StackServer, [])
{:ok, #PID<0.149.0>}

iex(2)> GenServer.call(gpid, :pop)
nil

iex(3)> GenServer.cast(gpid, {:push, 9})
:ok
iex(4)> GenServer.cast(gpid, {:push, "cat"})
:ok
iex(5)> GenServer.cast(gpid, {:push, 5})
:ok

iex(6)> GenServer.call(gpid, :pop)
5
iex(7)> GenServer.call(gpid, :pop)
"cat"
iex(8)> GenServer.call(gpid, :pop)
9
```

#### Tracing a Server's Execution

* `GenServer.start_link/3`; 3rd param is a set of options that can be `[debug: [:trace]]` during dev-process to log all message activity on console with `*DBG* ` prefix like...

```
iex(9)> {:ok, gpid} = GenServer.start_link(Sequence17.StackServer, [1,2,3,4], [debug: [:trace]])
{:ok, #PID<0.158.0>}

iex(10)> GenServer.call(gpid, :pop)
*DBG* <0.158.0> got call pop from <0.143.0>
*DBG* <0.158.0> sent 1 to <0.143.0>, new state [2,3,4]
1
```

* can have `:statistics` in list params passed to debug giving timestamps for `start_time, current_time, reductions, messages_in, messages_out`... here `reductions` measure amount of work done by server

* Erlang's `sys` module help interface to system messages sent in background between processes; param list passed to `debug` is function names to be called in `sys` for server's PID

> * so one can turn things on/off even after `GenServer.start_link/3` as `:sys.trace pid, true` to enable tracing
>
> * `:sys.trace pid, false` to disable tracing
>
> * `:sys.get_status pid` gives default formatting of status messages provided by GenServer; here `data:` part is customizable by defining `format_status` function

```
## this can work in Sequence17.Server
def format_status(_reason, [_pdict, state]) do
  [data: [{'State', "My current state is '#{inspect state}, :)"}]]
end
```

### GenServer Callbacks

* OTP works works by assuming that a module defines a number of Callback function; 6 in case of GenServer (OTP Protocol)

* Writing a GenServer in Erlang, would have to implement all 6

* adding `use GenServer` directive in Elixir creates default implementation of these 6 callback functions; can be overridden to add app-specific behavior

* `init(start_args)` called when starting a new server

> * `start_args` is 2nd param passed to `GenServer.start_link/2+`
>
> * should return `{:ok, state}` on success and `{:stop, reason}` on failure to start
>
> * could also return `{:ok, state, timeout}` where for any span of `timeout` ms with no message makes GenServer send the process a `:timeout` message to `handle_info`

* `handle_call(request, from, state)` invoked when `GenServer.call/2` is called

> * `from` is tuple of Client PID and unique tag; `state` is server state
>
> * returns `{:reply, result, new_state}` on success; default implementation stops server with a `:bad_call` error so write `handle_call` to manage all kind requests

* `handle_cast(request, state)` responds to `GenServer.cast/2`

> * `{:noreply, new_state}` on success
>
> * can also return `{:stop, reason, new_state}`
>
> * default implementation stops server with `:bad_cast`, so you know...

* `handle_info(info, state)` handles incoming messages which aren't cast or call requests; like `:timeout` message, termination messages from linked processes AND messages sent to PID using `send` bypass GenServer and routed here

* `terminate(reason, state)` invoked when to be terminated; not to be worried if using Supervisors

* `code_change(from_version, state, extra)` invoked to change state from older version to new version on hot swapping

* `format_status(reason, [pdict, state])` used to customize state display of server; conventional response `[data: [{'State', state_info}]]`

#### Call and Cast handler return Standardized Response

* some of these responses can contain `:hibernate` or `timeout` parameter

> * on `hibernate` the server state is removed from memory, recovered on next request (saving memory sacrificing CPU & I/O)
>
> * `_timeout` option by default is `:infinite`; can be a number of milliseconds too

* `{:noreply, new_state, [,:hibernate|_timeout]}` and `{:stop, reason, new_state}` are usable by both call and cast

* only call can use `{:reply, response, new_state [, :hibernate|timeout]}` and `{:stop, reason, reply, new_state}`


### Naming a Process

* can do local naming unique for all OTP processes on our node using `name:` option in `GenServer.start_link` as `{:ok, pid} = GenServer.start_link(Sequence17.StackServer, [], name: :seq17)`

> can then be used as `GenServer.cast(:seq17, {:push, 17})`; `:sys.get_status :seq17`


### Tidying Up The Interface

* our calls our ugly without wrapper around `GenServer` interface

> adding `External API` section [Sequence17.StackServer](./sequence17/lib/sequence17/stackserver.ex) allows usage as

```
iex(1)> Sequence17.StackServer.start([])
{:ok, #PID<0.184.0>}
iex(2)> Sequence17.StackServer.pop
nil
iex(3)> Sequence17.StackServer.push 2
:ok
iex(4)> Sequence17.StackServer.push 5
:ok
iex(5)> Sequence17.StackServer.pop
5
iex(6)> Sequence17.StackServer.pop
2
iex(7)> Sequence17.StackServer.pop
nil
```


### Making Our Server into a Component

> this ideology isn't mainstream

* The `Sequence17.StackServer` implementation puts 3 things in a single module; `API`, `Business Logic` & `logic implementation in a server`

* Implementing these separately, we'll put API in top at `lib/sequence.ex` module; implementation `impl.ex` and server logic `server.ex` under `lib/sequence`.

> Now with a complicated Business Logic, this approach would be more maanageable.

---

> To deal with Crashes, we'll look at Supervisors.

---
