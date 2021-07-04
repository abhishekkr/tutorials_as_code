
## Chapter.12 OTP

> adding a new app under umbrella project to create a Counter server

### Managing State with Processes

```
cd apps
mix new nfo --sup
cd nfo
```

* can create `counter.ex` for Counter server to count up/down

> it gives `inc/1`, `dec/1` & `val/1,2` functions; and `listen/1` which takes initial value from `start_link` & responds to up/down/value requests

```
defmodule Nfo.Counter do
  def inc(pid), do: send(pid, :inc)

  def dec(pid), do: send(pid, :dec)

  def val(pid, timeout \\ 5_000) do
    ref = make_ref()
    send(pid, {:val, self(), ref})

    receive do
      {^ref, val} -> val
    after
      timeout -> exit(:timeout)
    end
  end

  def start_link(init_val) do
    {:ok, spawn_link(fn -> listen(init_val) end)}
  end

  def listen(val) do
    receive do
      :inc ->
        listen(val+1)
      :dec ->
        listen(val-1)
      {:val, sender, ref} ->
        send(sender, {ref, val})
        listen(val)
    end
  end
end
```


### Building GenServers for OTP

* use provided GenServer capability for this instead and manage `receive` via `handle_cast/call` variants & callers

```
defmodule Nfo.Counter do
  use GenServer

  def inc(pid), do: GenServer.cast(pid, :inc)

  def dec(pid), do: GenServer.cast(pid, :dec)

  def val(pid), do: GenServer.call(pid, :val)

  def start_link(init_val), do: GenServer.start_link(__MODULE__, init_val)

  def init(init_val), do: {:ok, init_val}

  def handle_cast(:inc, val), do: {:noreply, val+1}

  def handle_cast(:dec, val), do: {:noreply, val-1}

  def handle_call(:val, _from, val), do: {:reply, val, val}
end
```

> usable as

```
iex(1)> {:ok, cnt} = Nfo.Counter.start_link(7)
{:ok, #PID<0.718.0>}
iex(2)> Nfo.Counter.inc(cnt)
:ok
iex(3)> Nfo.Counter.val(cnt)
8
iex(4)> Nfo.Counter.inc(cnt)
:ok
iex(5)> Nfo.Counter.val(cnt)
9
iex(6)> Nfo.Counter.dec(cnt)
:ok
iex(7)> Nfo.Counter.val(cnt)
```

* add Counter to Supervision tree `children = [ {Nfo.Counter, 0} ]`

* add a periodic tick to counter with `Process.send_after(self(), :tick, 1000)`

* can override `child_spec` function in GenServer to manage spawning with tweaks

* can use Agents to have an easier implementation with `start_link`, `stop`, `get`, `update` and `get_and_update`


### Designing an Information System with OTP

* add OTP-backed information service for Annotations; like pulling from an API like WolframAlpha

#### Planning our Supervision Stratgey

> to just need spawn few concurrent processes with no Supervision, can use **task**

```
tsk = Task.async(fn -> call_me() end)
Task.await(tsk)
```

* here we don't need to link failures; but require a Parent for cleanup & discoverability

* start our tasks with `Task.Supervisor.async_nolink` instead of `Task.async`

* one process to keep external system requests rate-limited, implementing cache system

> make `Nfo.Cache` & `Task.Supervisor` part of Nfo Supervision Tree

#### Building a Cache Server without Bottlenecks

> code: `apps/nfo/lib/nfo/cache.ex`

* on a query, all backends will be triggered.. results collected & select best one to be returned

> * here if all backend requests depend on same process for caching, that will be a Bottleneck
>
> can use ETS `Erlang Term Storage` for caching; acting as a single shared in-mem cache server

* using Module name for cache, makes it generic single cache for now

```
    |> :ets.new([
      :set,                         # type of ETS table
      :named_table,                 # find above table by name
      :public,                      # allow other process to access values as well
...
```

* add a simple sweep strategy to expire all cache at regular intervals

#### Using Tasks to Fetch Data

> code: `apps/nfo/lib/nfo.ex`
>
> * `Nfo.Result` a struct to use by backend to pass on result with their score
>
> * `Nfo.compute` to map async query to the backends

* add an interface which spawns for backends, fetch & cache response, picks best to return


### Building the Wolfram Info System

* add a behavior def `backend.ex` for `name/0` & `compute/2` callbacks

* add `{:sweet_xml, "~> 0.6.6"}` to mix deps for parsing Wolfram

* implement Wolfram behavior at `wolfram.ex` with hard-coded score for successful answer

```
iex(1)> Nfo.compute("what's elixir")
[
  %Task{
    owner: #PID<0.740.0>,
    pid: #PID<0.754.0>,
    ref: #Reference<0.2857031697.3879469058.93341>
  }
]
iex(2)> flush()
:ok
iex(8)> flush()
{#Reference<0.2857031697.3879469058.93341>,
 [
   %Nfo.Result{
     backend: Nfo.Wolfram,
     score: 95,
     text: "1 | noun | a sweet flavored liquid (usually containing a small amount of alcohol) used in compounding medicines to be taken by mouth in order to mask an unpleasant taste\n2 | noun | hypothetical substance that the alchemists believed to be capable of changing base metals into gold\n3 | noun | a substance believed to cure all ills"
   }
 ]}
{:DOWN, #Reference<0.2857031697.3879469058.93341>, :process, #PID<0.754.0>,
 :normal}
:ok
```

#### Monitoring Processes

> `:DOWN` message above is due to Process Monitor set up by Task library to detect backend crash; can be used explicitly

```
iex(1)> pid = spawn(fn -> :noop end)
#PID<0.948.0>
iex(2)> Process.monitor(pid)
#Reference<0.2857031697.3879469058.123616>
iex(3)> flush()
{:DOWN, #Reference<0.2857031697.3879469058.123616>, :process, #PID<0.948.0>,
 :noproc}
:ok

iex(4)> xpid = spawn(fn -> raise "force fail" end)
#PID<0.962.0>
[error] Process #PID<0.962.0> raised an exception
** (RuntimeError) force fail
    (stdlib 3.12.1) erl_eval.erl:678: :erl_eval.do_apply/6
iex(5)> Process.monitor(xpid)
#Reference<0.2857031697.3879469058.123906>
iex(6)> flush()
{:DOWN, #Reference<0.2857031697.3879469058.123906>, :process, #PID<0.962.0>,
 :noproc}
:ok
```

* these help backend crash friendly, not waiting on results which will never come

#### Working with Task Tools

* `Task.await` crash caller on time out; `Task.yield` blocks the caller returning result/error/nil; `Task.yield_many` would wait on all tasks taking no more than given time

> update `Nfo.compute` to yield for all results, collect useful results and kill other tasks for precaution, sort by score & pick upto limit

#### Caching Results

> modify `Nfo.compute` to read from cache for each backend; first we collect all results already cached then run Tasks for uncached and append it with cached


### Integrating OTP Services with Channels

* add `{:info_sys, in_umbrella: true},` to `mix.exs` under `videologue_web`

* add a task `compute_additional_info(annotation, socket)` after broadcast in `handle_in` to broadcast backend's result with a backend name user

* add seed for backend user name creation


### Wrapping Up

> [Chapter-13 Testing Channels and OTP](./chapter-13.md)

---
