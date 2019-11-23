
## OTP Concurrency

> sample code at [p22_otp_concurrency.exs](p22_otp_concurrency.exs)
>
> focus on `GenServers`, [official doc](https://hexdocs.pm/elixir/GenServer.html#content)

* OTP server is a module with GenServer behavior implementing a set of callbacks, a process handling a message iteratively passing along update state


### GenServer

* to use it, need to specify `use GenServer`, to link processes use `GenServer.start_link/3` passing module, initial arguments and options

* arguments passed to `GenServer.init/1` which configures initial state

> `SimpleQ` in code sample provides above


#### Synchronous Functions

* calling a sync function is implemented via `GenServer.handle_call/3` callback which takes request, caller's PID and existing state; returns a tuple `{:reply, response, state}`

> `SyncQ` in code sample provides this usage exposing sync calls for queue, enqueue and dequeue

```
iex(1)> c("p22_otp_concurrency.exs")
[AsyncQ, SimpleQ, SyncQ]

iex(2)> SyncQ.start_link([])
{:ok, #PID<0.119.0>}

iex(3)> SyncQ.dequeue
nil

iex(4)> SyncQ.queue
[]

iex(5)> SyncQ.enqueue 10
:ok

iex(6)> SyncQ.queue |> IO.inspect(charlists: false)
[10]

iex(7)> SyncQ.enqueue 100
:ok

iex(8)> SyncQ.queue |> IO.inspect(charlists: false)
[10, 100]

iex(9)> SyncQ.dequeue
10

iex(10)> SyncQ.queue |> IO.inspect(charlists: false)
[100]
```


#### Asynchronous Functions

* async requests via `GenServer.handle_cast/2` callback, it doesn't receive caller and not expected to reply

> `Async.enqueue/1` is implemented for async calls in sample code

```
iex(2)> AsyncQ.start_link([1,2,3])
{:ok, #PID<0.119.0>}

iex(3)> AsyncQ.dequeue
1

iex(4)> AsyncQ.queue() |> IO.inspect(charlists: false)
[2, 3]

iex(5)> AsyncQ.enqueue(100)
:ok

iex(6)> AsyncQ.queue() |> IO.inspect(charlists: false)
[2, 3, 100]
```

---
