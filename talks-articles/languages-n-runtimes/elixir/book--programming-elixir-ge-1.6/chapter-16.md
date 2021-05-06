
## Chapter.16 Nodes - The Key to Distributing Services

> running any Erlang code, would be running on a node by Erlang VM (**Beam**)
>
> it's like a mini OS with its own events, process scheduling, memory management, naming services, interprocess communication
>
> alongwith all this, nodes can connect to each other on same or network connected computer


### Naming Nodes

* identify node name as `Node.self`; can give a custom name to node as `iex --sname mename`

* `--name mename@host-nam.local` fully-qualified or `--sname mename` short name switch could be used

* returned name is an `atom`; in quotes as some characters not allowed in literal atom

* can run 2 nodes as easy as running 2 IEx with different `--sname` values

> * they don't know of each other by default
>
> * connect via `Node.connect :"other-node@host-nam.local"`
>
> * now `Node.list` shall return connected Nodes
>
> * can run a function via `spawn(NODE_IDENTIFIER, function)` on remote or even same node

```
% iex --sname alice
## one one terminal

## then on other
% iex --sname bob
Erlang/OTP 22 [erts-10.7.2.9] ...

Interactive Elixir (1.9.2) - ...

iex(bob@lixir)1> foo = fn -> IO.inspect(Node.self) end
#Function<21.126501267/0 in :erl_eval.expr/5>

iex(bob@lixir)2> spawn(foo)
:"bob@lixir"
#PID<0.118.0>

iex(bob@lixir)3> Node.spawn(:"bob", foo)
#PID<0.120.0>
14:05:34.063 [warn]  ** Can not start :erlang::apply,[#Function<21.126501267/0 in :erl_eval.expr/5>, []] on :bob **
nil

iex(bob@lixir)5> Node.spawn(:"bob@lixir", foo)
:"bob@lixir"
#PID<0.123.0>

iex(bob@lixir)6> Node.spawn(:"alice@lixir", foo)
#PID<15627.120.0>
:"alice@lixir"
```

> we see when Node spawns at another Node; the PID returned has a `>0` value at first place in dot separated tokens
>
> the output from Remote node also appears on Node that called irrespective the process was ran on Remote; even if it was STDOUT stuff

#### Nodes, Cookies and Security

* can use cookie (just a random string), which gets compared to node's own cookie before allowing a call from remote node

* can be set via `--cookie another-one`; checked via `Node.get_cookie`

* if cookies are different for two nodes; the initiator gets a `false` on attempt

* Erlang stores a random cookie at `.erlang.cookie` if not provided one; thus on a particular machine if not provided otherwise.. all Nodes will share the same cookie


### Naming Your Processes

* PID displayed as 3 numbers; has first as Node-ID (always 0 on current node, when to PID to be sent to another node the first number is assigned number of node on which process resides) and rest two as low and high bits of Process-ID

* The PID sharing works great if callback PID is given to generator; but how can callback find a generator in minimal setup system. One way is generator registering its PID with a convention set name; callback on other node can use it to infer details.

* example module `Ticker`; `Ticker.start/0` runs the server & `Client.start` triggers the client; then `Ticker.register/1` to register a client

> this registration mechanism decouples client from server for a some complexities
>
> although this sample doesn't have error-handling or a way to clean terminate; need to be evolved for that

#### When to Name Processes

* naming anything records a Global State which can be troublesome; to avoid clashes General Rule is to only Register your process names when your Application starts


### Input, Output, PIDs and Nodes

* an open file/device is identifies by PID of its I/O server

* example `IO.puts`

> * uses `map_device` to map a device to erlang devce; default device used is returned by functon `:erlang.group_leader`

> * start 2 nodes A & B; connect A to B and make A `:global.register_name(:a, :erlang.group_leader)`
>
> * so when you `a = :global.whereis_name(:a)` at B and run `IO.puts(a, "OK")`; the output is available at A


### Nodes are the Basis of Distribution

> writing a happy-path code is easy; will need help writing bullet-proof, scalable, hot-swappable, world-beating app AND that help is OTP


---
