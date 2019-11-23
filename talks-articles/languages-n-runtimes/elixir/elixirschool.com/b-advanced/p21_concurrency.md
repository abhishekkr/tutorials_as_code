
## Concurrency

> concurrency model relies on Actors, a contained process communicating with others using message-passing
>
> [sample code](p21_concurrency.exs)


### Processes

> are lightwieght running across all cores; behave like native threads but can run thousands concurrently

* easiest way to eval a new process async, `spawn/3` can be used for anon or named functions which returns `PID`

* example `Xyz.dbl/1` in sample code

```
iex> Xyz.dbl(10)
20
iex> spawn(Xyz, :dbl, [10])
#PID<0.116.0>
```

#### Message Passing

* `send/2` and `receive do...end` can be used to send and listen with matching

* if no match is found in `receive`, the execution continues uninterrupted

* example `Xyz.listen/0` in sample code is recursive allowing it to run forever waiting to receive

```
iex> p = spawn(Xyz, :listen, [])
#PID<0.133.0>

iex> send(p, {:ok, "Jane"})
Hey, Jane.
{:ok, "Jane"}

iex> send(p, {:error, "sorry Jane"})
error: sorry Jane
{:error, "sorry Jane"}
```

#### Process Linking

* `spawn` doesn't allow originating process to know if spawned process crashed, `spawn_link` allows linking such information across processes

* in `Xyz.clean_crash/0`, `spawn_link` followed receive matchs on process trapped exit


#### Process Monitoring

* keeping check on spawned process without linking 2 processes, `spawn_monitor` is used

* `spawn_monitor` returns `reference` along with `PID`, that allows get a message on spawned process crash without explit signal traps


### Agents

* an abstraction around background processes with state (set to function's return value)

* accessible from other processes within our application and node

```
iex> {:ok, agent} = Agent.start_link(fn -> [11, 22, 33] end)
{:ok, #PID<0.201.0>}

iex> Agent.update(agent, fn(state) -> state ++ [4, 5] end)
:ok

iex> Agent.get(agent, &(&1))
[11, 22, 33, 4, 5]
```

* can have named agent for declarative usage, instead of its PID

```
iex> {:ok, agent} = Agent.start_link(fn -> Date.utc_today end, name: Today)
{:ok, #PID<0.211.0>}

iex> Agent.get(Today, &(&1))
~D[2018-12-31]
```


### Tasks

* way to execute background function and retrieve value later, can be useful to not get blocked at resource expensive ops

> `Task.async` to issue a task, `Task.await` to wait for task

```
iex> defmodule INeedTime, do: ( def plus100(x), do: (:timer.sleep(200) ; x + 100) )
{:module, INeedTime,
 <<70, 79, 82, 49, 0, 0, 5, 28, 66, 69, 65, 77, 65, 116, 85, 56, 0, 0, 0, 145,
   0, 0, 0, 16, 16, 69, 108, 105, 120, 105, 114, 46, 73, 78, 101, 101, 100, 84,
   105, 109, 101, 8, 95, 95, 105, 110, 102, ...>>, {:plus100, 1}}

iex> mytask = Task.async(INeedTime, :plus100, [1])
%Task{
  owner: #PID<0.105.0>,
  pid: #PID<0.231.0>,
  ref: #Reference<0.3042913939.3022258178.5296>
}

iex> Date.utc_today
~D[2019-11-22]

iex> Task.await(mytask)
101
```

---
